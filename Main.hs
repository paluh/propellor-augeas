{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when, void)
import           Control.Monad.Reader (liftIO)
import           Data.ByteString (concat, empty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (fromMaybe)
import           Propellor.Property.Augeas (Augeas, AugeasConfig, AugeasResult(..), augSet, augMatch,
                                            augSave, augeasRoot, defaultConfig, runAugeas)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Posix ((</>))

-- dirty/tesing version of ssh allowed users editing
setupNewUser :: ByteString.ByteString -> Augeas ()
setupNewUser username = do
  let userQuery = ByteString.concat [ "/files/etc/ssh/sshd_config/AllowUsers/*[.=\""
                                    , username
                                    , "\"]"]
  r <- augMatch userQuery
  a <- augMatch "/files/etc/ssh/sshd_config/AllowUsers/*"
  liftIO $ print a
  case r of
    AugeasResult mu -> do
      liftIO $ print mu
      liftIO $ print (null $ fromMaybe [] mu)
      when (null $ fromMaybe [] mu)
        (void $ augSet "/files/etc/ssh/sshd_config/AllowUsers/01" username)
      void augSave

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  runAugeas defaultConfig{ augeasRoot=Char8.pack (cwd </> "testroot") }
            (setupNewUser "test")
  return ()
