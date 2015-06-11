{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Propellor.Property.Augeas where

import           Data.ByteString (ByteString, empty)
import           System.Augeas (aug_init, aug_match, aug_save, aug_set, Augeas, AugRet(..), AugFlag,
                                save_newfile, enable_span, aug_get)
import           Foreign (Ptr, withForeignPtr)

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

data AugeasCommand t where
  Set :: Path -> ByteString -> AugeasCommand AugRet
  Get :: Path -> AugeasCommand ByteString
  Match :: Path -> AugeasCommand [ByteString]

data AugeasFailureReason = AugeasCommandError AugRet [(Path, ErrorMessage)]
                         | AugeasInitializationFailed

data AugeasResult t = AugeasResult t
                    | AugeasFailure AugeasFailureReason

withAugeasPtr :: AugeasConfig -> (Ptr Augeas -> AugeasResult t) -> IO (AugeasResult t)
withAugeasPtr c f = do
  maybeAugeasPtr <- aug_init (augeasRoot c) (augeasLoadPath c) (augeasFlags c)
  case maybeAugeasPtr of
    Just augeasForeignPtr -> (withForeignPtr augeasForeignPtr (return . f))
    Nothing -> return (AugeasFailure AugeasInitializationFailed)


