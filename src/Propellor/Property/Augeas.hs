{-# LANGUAGE OverloadedStrings #-}

module Propellor.Property.Augeas where

import           Control.Monad.Reader (ask, liftIO, ReaderT, runReaderT)
import           Data.ByteString (ByteString, empty)
import           Foreign (Ptr, withForeignPtr)
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
type ErrorMessage = ByteString

data AugeasFailureReason = AugeasCommandError [(Path, ErrorMessage)]
                         | AugeasSaveError [(Path, ErrorMessage)]
                         | AugeasInitializationFailed
                         deriving (Show)

data AugeasResult t = AugeasResult t
                    | AugeasFailure AugeasFailureReason
                    deriving (Show)

type Augeas = ReaderT (Ptr A.Augeas) IO

augSet :: Path -> Value -> Augeas (AugeasResult AugRet)
augSet p v = do
  aPtr <- ask
  r <- liftIO $ aug_set aPtr p v
  return . AugeasResult $ r

augGet :: Path -> Augeas (AugeasResult (Maybe String))
augGet p = do
  aPtr <- ask
  m <- liftIO $ aug_get aPtr p
  case m of
    Left _  -> return . AugeasFailure $ AugeasCommandError []
    Right r -> return . AugeasResult $ r

augMatch :: Path -> Augeas (AugeasResult (Maybe [String]))
augMatch p = do
  aPtr <- ask
  (r, m) <- liftIO $ aug_match aPtr p
  return . AugeasResult $ m

augSave :: Augeas (AugeasResult ())
augSave = do
  aPtr <- ask
  r <- liftIO $ aug_save aPtr
  if r == A.error
    then return $ AugeasFailure . AugeasSaveError $ []
    else return $ AugeasResult ()

withAugeasPtr :: AugeasConfig -> (Ptr A.Augeas -> IO a) -> IO (Maybe a)
withAugeasPtr c f = do
  maybeAugeasPtr <- aug_init (augeasRoot c) (augeasLoadPath c) (augeasFlags c)
  case maybeAugeasPtr of
    Just augeasForeignPtr -> withForeignPtr augeasForeignPtr (\a -> Just <$> f a)
    Nothing -> return Nothing

runAugeas :: AugeasConfig -> Augeas a -> IO (Maybe a)
runAugeas augConf actions =
  withAugeasPtr augConf (runReaderT actions)
