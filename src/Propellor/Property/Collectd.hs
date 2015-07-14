{-# LANGUAGE OverloadedStrings #-}
module Propellor.Property.Collectd where

import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8 (pack)
import           Data.Optional (Optional(..))
import           Propellor.Property.Augeas (Augeas, AugeasBase, augGet, augMatch, AugeasFailure,
                                            coerceAugeasFailure, evalAugeas)

type Filter = String
type Seconds = Int
type Iterations = Int

data IncludePattern = Simple String | Include FilePath Filter

data Globals =
       Globals
         { baseDir :: Optional FilePath
         , autoLoadPlugin :: Optional Bool
         , includePath :: Optional [IncludePattern]
         , pidFile :: Optional FilePath
         , pluginDir :: Optional FilePath
         , typesDB :: Optional [FilePath]
         , interval :: Optional Seconds
         , timeout :: Optional Iterations
         , readThreads :: Optional Int
         , writeThreads :: Optional Int
         , writeQueueLimitHigh :: Optional Int
         , writeQueueLimitLow :: Optional Int
         , hostname :: Optional
                        String
          -- PostCacheChain ChainName
          -- FQDNLookup true|false PreCacheChain ChainName
         }
optFromMaybe :: Maybe a -> Optional a
optFromMaybe (Just a) = Specific a
optFromMaybe Nothing = Default

data CollectdError = ConfigParseError String | AF AugeasFailure
type Collectd a = AugeasBase CollectdError a

liftAugeas a = coerceAugeasFailure a AF

-- getGlobal :: String -> Collectd (Maybe String) -- (Either ConfigError (Optional a))
getGlobal name = do
    e <- liftAugeas $ augGet (ByteString.concat [ "/files/etc/collectd.conf/directive[.=\""
                                          , pack name
                                          , "\"]/arg"
                                          ])
    return . optFromMaybe $ e
    --((Right . read) <$>) . optFromMaybe <$>
-- globals :: (Globals -> Globals) -> Augeas Globals
-- globals f = do
--   baseDir <- getGlobalString "BaseDir"
--   augeasLoadPlugin <- getGlobalString "AutoLoadPlugin"
--   includePath <- getGlobalString "IncludePath"
--   Globals {
--     baseDir = 

