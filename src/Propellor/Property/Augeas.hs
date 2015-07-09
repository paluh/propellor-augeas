{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Propellor.Property.Augeas where

import           Control.Monad (forM, sequence)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT, left, runEitherT)
import           Control.Monad.Trans.State.Strict (evalStateT, get, gets, put, modify, StateT(..), runStateT, state)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (catMaybes, fromMaybe)
import           Foreign (Ptr, nullPtr, withForeignPtr)
import           Propellor.Types (Propellor(..))
import           Propellor.Types.Result (Result(..))
import           System.Augeas (aug_init, aug_match, aug_save, aug_set, AugRet(..), AugFlag,
                                save_newfile, enable_span, aug_get, AugMatch(..))
import qualified System.Augeas as A

data AugeasConfig =
  AugeasConfig { augeasRoot :: ByteString
               , augeasLoadPath :: ByteString
               , augeasFlags :: [AugFlag]
               }
  deriving (Show)

-- I'm not sure how to pass NULL and
-- not overwrite internal augeas defaults
defaultConfig :: AugeasConfig
defaultConfig = AugeasConfig "/" empty [enable_span]

type Path = ByteString
type Value = ByteString

type ModifiedFiles = [String]
type ErrorMessage = String
type AugeasErrors = [(String, Maybe ErrorMessage)]
data AugeasFailure = AugeasCommandError ModifiedFiles AugeasErrors
                   | AugeasInitializationFailed
                   deriving (Show)

data AugeasSession =
  AugeasSession
    { augPtr        :: Ptr A.Augeas
    , modifiedFiles :: [String]
    }

type Augeas a = EitherT AugeasFailure (StateT AugeasSession IO) a

gets' :: (AugeasSession -> c) -> Augeas c
gets' = lift . gets
put' :: AugeasSession -> Augeas ()
put' = lift . put

commandError :: Augeas a
commandError = do
  aPtr <- gets' augPtr
  e <- fromMaybe [] <$> augMatch "/augeas//errors/*"
  m <- lift . gets $ modifiedFiles
  left (AugeasCommandError m e)

augSet :: Path -> Value -> Augeas AugRet
augSet p v = do
  aPtr <- gets' augPtr
  liftIO $ aug_set aPtr p v

augGet :: Path -> Augeas (Maybe String)
augGet p = do
  aPtr <- gets' augPtr
  m <- liftIO $ aug_get aPtr p
  case m of
    Left _  -> commandError
    Right v -> return v

augMatch :: Path -> Augeas (Maybe [(String, Maybe String)])
augMatch p = do
  aPtr <- gets' augPtr
  (r, m) <- liftIO $ aug_match aPtr p
  case m of
    Just paths -> Just <$> sequence [(,) p <$> augGet (Char8.pack p) | p <- paths]
    Nothing  -> return Nothing

augSave :: Augeas ()
augSave = do
  aPtr <- gets' augPtr
  r <- liftIO $ aug_save aPtr
  if r == A.error
    then commandError
    else do
      mf <- fromMaybe [] <$> augMatch "/augeas/events/saved"
      let mf' = [v | (_, Just v) <- mf]
      lift . modify $ (\s -> s{modifiedFiles = mf' ++ modifiedFiles s})
      return ()

withAugeasPtr :: AugeasConfig -> (Ptr A.Augeas -> IO a) -> IO (Maybe a)
withAugeasPtr c f = do
  maybeAugeasPtr <- aug_init (augeasRoot c) (augeasLoadPath c) (augeasFlags c)
  case maybeAugeasPtr of
    Just augeasForeignPtr -> withForeignPtr augeasForeignPtr (\a -> Just <$> f a)
    Nothing -> return Nothing


type AugeasResult a = Either AugeasFailure a

-- return augeas session with augeas pointer cleared up
runAugeas :: AugeasConfig -> Augeas a -> IO (AugeasResult a, AugeasSession)
runAugeas augConf actions = do
  mr <- withAugeasPtr augConf (\p -> runStateT (runEitherT actions) (AugeasSession p []))
  case mr of
     Nothing -> return (Left AugeasInitializationFailed, AugeasSession {augPtr = nullPtr, modifiedFiles = []})
     Just (r, s) -> return (r, s{ augPtr = nullPtr })

evalAugeas :: AugeasConfig -> Augeas a -> IO (AugeasResult a)
evalAugeas c a = (fst <$>) (runAugeas c a)

execAugeas :: AugeasConfig -> Augeas a -> IO AugeasSession
execAugeas c a = (snd <$>) (runAugeas c a)

-- propellor integration

augeasAction :: AugeasConfig -> Augeas a -> Propellor Result
augeasAction cfg as = do
  (result, session) <- liftIO $ runAugeas cfg as
  return $ case result of
    Left e -> FailedChange
    Right _ ->
      if not . null . modifiedFiles $ session
      then MadeChange
      else NoChange
