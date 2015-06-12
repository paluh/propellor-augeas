{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when, void)
import           Data.ByteString (concat, empty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (fromMaybe)
import           Propellor.Property.Augeas (Augeas, AugeasConfig, AugeasResult(..), augSet, augMatch,
                                            augSave, augeasRoot, defaultConfig, runAugeas)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Posix ((</>))

-- dirty/tesing version of ssh allowed users editing
allowUser :: ByteString.ByteString -> Augeas ()
allowUser username = do
  let userQuery = ByteString.concat [ "/files/etc/ssh/sshd_config/AllowUsers/*[.=\""
                                    , username
                                    , "\"]"]
  ps <- fromMaybe [] <$> augMatch userQuery
  when (null ps)
      (void $ augSet "/files/etc/ssh/sshd_config/AllowUsers/01" username)
  augSave

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  r <- runAugeas defaultConfig { augeasRoot = Char8.pack (cwd </> "testroot") } (allowUser "bar")
  print r
  return ()
