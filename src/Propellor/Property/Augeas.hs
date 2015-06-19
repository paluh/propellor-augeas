{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Propellor.Property.Augeas where

import           Control.Monad (forM, sequence)
import           Control.Monad.Trans.Either (EitherT, left, runEitherT)
import           Control.Monad.Trans.State.Strict (evalStateT, get, gets, put, modify, StateT(..), runStateT, state)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (catMaybes, fromMaybe)
import           Foreign (Ptr, withForeignPtr)
import           Propellor.Types (Propellor(..))
import           Propellor.Types.Result (Result(..), ToResult(..))
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

type Augeas a = StateT AugeasSession (EitherT AugeasFailure IO) a

commandError :: Augeas a
commandError = do
  aPtr <- gets augPtr
  e <- fromMaybe [] <$> augMatch "/augeas//errors/*"
  m <- gets modifiedFiles
  StateT (\_ -> left (AugeasCommandError m e))

augSet :: Path -> Value -> Augeas AugRet
augSet p v = do
  aPtr <- gets augPtr
  liftIO $ aug_set aPtr p v

augGet :: Path -> Augeas (Maybe String)
augGet p = do
  aPtr <- gets augPtr
  m <- liftIO $ aug_get aPtr p
  case m of
    Left _  -> commandError
    Right v -> return v

augMatch :: Path -> Augeas (Maybe [(String, Maybe String)])
augMatch p = do
  aPtr <- gets augPtr
  (r, m) <- liftIO $ aug_match aPtr p
  case m of
    Just paths -> Just <$> sequence [(,) p <$> augGet (Char8.pack p) | p <- paths]
    Nothing  -> return Nothing

augSave :: Augeas ()
augSave = do
  aPtr <- gets augPtr
  r <- liftIO $ aug_save aPtr
  if r == A.error
    then commandError
    else do
      mf <- fromMaybe [] <$> augMatch "/augeas/events/saved"
      let mf' = [v | (_, Just v) <- mf]
      modify (\s -> s{modifiedFiles = mf' ++ modifiedFiles s})
      return ()

withAugeasPtr :: AugeasConfig -> (Ptr A.Augeas -> IO a) -> IO (Maybe a)
withAugeasPtr c f = do
  maybeAugeasPtr <- aug_init (augeasRoot c) (augeasLoadPath c) (augeasFlags c)
  case maybeAugeasPtr of
    Just augeasForeignPtr -> withForeignPtr augeasForeignPtr (\a -> Just <$> f a)
    Nothing -> return Nothing


type AugeasResult a = Either AugeasFailure (a, ModifiedFiles)

runAugeas :: AugeasConfig -> Augeas a -> IO (AugeasResult a)
runAugeas augConf actions = do
  let actions' = do {
    a <- actions;
    mf <- gets modifiedFiles;
    return (a, mf);
  }
  mr <- withAugeasPtr augConf (\p -> runEitherT $ evalStateT actions' (AugeasSession p []))
  case mr of
    Nothing -> return . Left $ AugeasInitializationFailed
    Just r -> return r


-- propellor integration

instance ToResult (AugeasResult a) where
  toResult (Left _) = FailedChange
  toResult (Right (_, modified)) =
    if null modified
      then MadeChange
      else NoChange

augeasAction :: AugeasConfig -> Augeas a -> Propellor Result
augeasAction cfg as = do
  augResult <- liftIO $ runAugeas cfg as
  return . toResult $ augResult
