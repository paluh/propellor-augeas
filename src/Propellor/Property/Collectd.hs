{-# LANGUAGE OverloadedStrings #-}
module Propellor.Property.Collectd where

import           Control.Monad.Trans.Either (left)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8 (pack)
import           Data.Maybe (mapMaybe, fromMaybe, maybe)
import           Data.Optional (Optional(..))
import           System.Augeas (AugRet)
import           Propellor.Property.Augeas (Augeas, AugeasBase, AugeasConfig, augGet, augMatch, augRm,
                                            augSave, augSet, AugeasFailure, AugeasSession,
                                            coerceAugeasFailure, runAugeasBase)
import           Text.Read (readMaybe)

type Seconds = Int
type Iterations = Int

type Filter = String
data Include =
  Include FilePath
          Filter
  deriving (Show)

data Globals =
  Globals {autoLoadPlugin :: Optional Bool
          ,baseDir :: Optional FilePath
          ,hostname :: Optional String
          ,includePath :: Optional Include
          ,interval :: Optional Seconds
          ,pidFile :: Optional FilePath
          ,pluginDir :: Optional FilePath
          ,readThreads :: Optional Int
          ,typesDB :: Optional [FilePath]
          ,timeout :: Optional Iterations
          ,writeThreads :: Optional Int
          ,writeQueueLimitHigh :: Optional Int
          ,writeQueueLimitLow :: Optional Int}
  -- PostCacheChain ChainName
  -- FQDNLookup true|false PreCacheChain ChainName
  deriving (Show)

type VarName = String
type VarValue = String

data CollectdError = ConfigParseError VarName VarValue
                   | AugeasFailure AugeasFailure
  deriving Show

type Collectd a = AugeasBase CollectdError a

optFromMaybe :: Maybe a -> Optional a
optFromMaybe = maybe Default Specific

liftAugeas a = coerceAugeasFailure a AugeasFailure

getMultiValueGlobal :: ([String] -> Maybe a) -> String -> Collectd (Optional a)
getMultiValueGlobal f name = do
  k2v <- liftAugeas $ augMatch
                        (ByteString.concat
                           ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/*"])
  let v = mapMaybe snd . fromMaybe [] $ k2v
  return . optFromMaybe $ f v

getSimpleGlobal :: (String -> Maybe a) -> String -> Collectd (Optional a)
getSimpleGlobal f name = do
  e <- liftAugeas $ augGet
                      (ByteString.concat
                         ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/arg"])
  case e of
    Nothing -> return Default
    Just value ->
      case f value of
        Nothing -> left (ConfigParseError name value)
        r       -> return . optFromMaybe $ r

parseBool :: String -> Maybe Bool
parseBool "true"  = Just True
parseBool "false" = Just False
parseBool _       = Nothing

getBool :: VarName -> Collectd (Optional Bool)
getBool = getSimpleGlobal parseBool

getInt :: VarName -> Collectd (Optional Int)
getInt = getSimpleGlobal (readMaybe::(String -> Maybe Int))

getString :: VarName -> Collectd (Optional String)
getString = getSimpleGlobal Just

-- TODO: add support for second form of Include
-- <Include "/etc/collectd.d">
--   Filter "*.conf"
-- </Include>
getIncludePath :: Collectd (Optional Include)
getIncludePath =
  getMultiValueGlobal pg "Include"
  where
    pg [p] = Just $ Include p "*"
    pg [p, f] = Just $ Include p f
    pg _ = Nothing

getGlobals :: Collectd Globals
getGlobals = do
  autoLoadPlugin' <- getBool "AutoLoadPlugin"
  baseDir' <- getString "BaseDir"
  hostname' <- getString "Hostname"
  includePath' <- getIncludePath
  interval' <- getInt "Interval"
  pidFile' <- getString "PIDFile"
  pluginDir' <- getString "PluginDir"
  readThreads' <- getInt "ReadThreads"
  timeout' <- getInt "Timeout"
  typesDB' <- getMultiValueGlobal Just "TypesDB"
  writeQueueLimitHigh' <- getInt "WriteQueueLimitHigh"
  writeQueueLimitLow' <- getInt "WriteQueueLimitLow"
  writeThreads' <- getInt "WriteThreads"
  return
    Globals
      { autoLoadPlugin = autoLoadPlugin'
      , baseDir = baseDir'
      , pidFile = pidFile'
      , pluginDir = pluginDir'
      , hostname = hostname'
      , interval = interval'
      , includePath = includePath'
      , readThreads = readThreads'
      , timeout = timeout'
      , typesDB = typesDB'
      , writeThreads = writeThreads'
      , writeQueueLimitHigh = writeQueueLimitHigh'
      , writeQueueLimitLow = writeQueueLimitLow'
      }

setSimpleGlobal :: (a -> String) -> VarName -> Optional a -> Collectd ()
setSimpleGlobal p name (Specific value) = do
  let value' = p value
  -- set /files/etc/apache2/sites-available/foo/VirtualHost/directive "ServerAdmin"
  -- set /files/etc/apache2/sites-available/foo/VirtualHost/*[self::directive="ServerAdmin"]/arg "admin@example.com"
  liftAugeas $ augRm
                 (ByteString.concat
                    ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]"])
  liftAugeas $ augSet "/files/etc/collectd.conf/directive[0]" (pack name)
  liftAugeas $ augSet
                 (ByteString.concat
                    ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/arg[1]"])
                 (pack value')
  return ()
setSimpleGlobal p name Default = do
  liftAugeas $ augRm
                 (ByteString.concat
                    ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]"])
  return ()

setString :: VarName -> Optional String -> Collectd ()
setString = setSimpleGlobal id

setInt :: VarName -> Optional Int -> Collectd ()
setInt = setSimpleGlobal (show :: (Int -> String))

setBool :: VarName -> Optional Bool -> Collectd ()
setBool = setSimpleGlobal (\b -> if b then "true" else "false")

setGlobals :: Globals -> Collectd ()
setGlobals Globals
  { autoLoadPlugin = autoLoadPlugin'
  , baseDir = baseDir'
  , pidFile = pidFile'
  , pluginDir = pluginDir'
  , hostname = hostname'
  , interval = interval'
  , includePath = includePath'
  , readThreads = readThreads'
  , timeout = timeout'
  , typesDB = typesDB'
  , writeThreads = writeThreads'
  , writeQueueLimitHigh = writeQueueLimitHigh'
  , writeQueueLimitLow = writeQueueLimitLow'
  } = do
  setBool "AutoLoadPlugin" autoLoadPlugin'
  setString "BaseDir" baseDir'
  setString "Hostname" hostname'
  -- includePath' <- getIncludePath
  setInt "Interval" interval'
  setString "PIDFile" pidFile'
  setString "PluginDir" pluginDir'
  setInt "ReadThreads" readThreads'
  setInt "Timeout" timeout'
  -- typesDB' <- getMultiValueGlobal Just "TypesDB"
  setInt "WriteQueueLimitHigh" writeQueueLimitHigh'
  setInt "WriteQueueLimitLow" writeQueueLimitLow'
  setInt "WriteThreads" writeThreads'
  return ()

runCollectd :: AugeasConfig -> Collectd a -> IO (Either CollectdError a, AugeasSession)
runCollectd = runAugeasBase AugeasFailure

evalCollectd :: AugeasConfig -> Collectd a -> IO (Either CollectdError a)
evalCollectd c a = (fst <$>) (runCollectd c a)

execCollectd :: AugeasConfig -> Collectd a -> IO AugeasSession
execCollectd c a = (snd <$>) (runCollectd c a)

updateGlobals :: (Globals -> Globals) -> Collectd ()
updateGlobals f = do
  globals <- getGlobals
  setGlobals . f $ globals
  liftAugeas augSave
