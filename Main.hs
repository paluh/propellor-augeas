{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when, void)
import           Data.ByteString (concat, empty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (fromMaybe)
import           Propellor.CmdLine (defaultMain)
import           Propellor.PropAccum (host, (&))
import           Propellor.Property.Augeas (Augeas, augeasAction, AugeasConfig, AugeasResult(..), augSet, augMatch,
                                            augSave, augeasRoot, defaultConfig, runAugeas)
import           Propellor.Property (property)
import           Propellor.Types (Host, Property, NoInfo)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Posix ((</>))

type Username = ByteString.ByteString

-- dirty/tesing version of ssh allowed users editing
allowUser :: Username -> Augeas ()
allowUser username = do
  let userQuery = ByteString.concat [ "/files/etc/ssh/sshd_config/AllowUsers/*[.=\""
                                    , username
                                    , "\"]"]
  ps <- fromMaybe [] <$> augMatch userQuery
  when (null ps)
      (void $ augSet "/files/etc/ssh/sshd_config/AllowUsers/01" username)
  augSave

allowUserProperty :: AugeasConfig -> Username -> Property NoInfo
allowUserProperty augConf username =
  property ("Allow user \"" ++ Char8.unpack username ++ "\" to login through ssh")
    (augeasAction augConf (allowUser username))

-- main :: IO ()
-- main = do
--   cwd <- getCurrentDirectory
--   let testingConfig = defaultConfig { augeasRoot = Char8.pack (cwd </> "testroot") }
-- 
--   -- r <- runAugeas defaultConfig { augeasRoot = Char8.pack (cwd </> "testroot") } (allowUser "bar")
--   print r
--   return ()

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let testingConfig = defaultConfig { augeasRoot = Char8.pack (cwd </> "testroot") }
  defaultMain [ host "localhost" & allowUserProperty testingConfig "brandnew" ]
