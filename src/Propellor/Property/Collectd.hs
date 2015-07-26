{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Propellor.Property.Collectd where

import           Control.Monad (mapM_, void)
import           Control.Monad.Trans.Either (EitherT(..), left, runEitherT)
import           Control.Monad.Trans.State.Strict (evalStateT, get, gets, put, modify, StateT(..), runStateT, state)

import           Control.Monad.Trans.Reader (ask, ReaderT(..), reader, runReaderT)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8 (pack)
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe, fromMaybe, maybe)
import           Data.Optional (Optional(..))
import           System.Augeas (AugRet)
import           Propellor.Property.Augeas (Augeas, AugeasBase, AugeasConfig, augGet, augMatch, augRm,
                                            augSave, augSet, AugeasFailure, AugeasSession,
                                            coerceAugeasFailure, runAugeasBase)
import           Text.Read (readMaybe)


-- From httpd.aug (collectd.aug uses the same parser):
--
--  Apache configuration is represented by two main structures, nested sections
--  and directives. Sections are used as labels, while directives are kept as a
--  value. Sections and directives can have positional arguments inside values
--  of "arg" nodes. Arguments of sections must be the firsts child of the
--  section node.

type ConfigName = String

type ConfigPath = String

type ConfigArgument = String

data ConfigStruct = Directive ConfigName [ConfigArgument]
                  | Section ConfigName [ConfigArgument] [ConfigStruct]
                  deriving (Show)

left' :: CollectdError -> Collectd a
left' = ReaderT . const . left

augRm' = liftAugeas . augRm . pack
augMatch' = liftAugeas . augMatch . pack
augSet' p = liftAugeas . augSet (pack p) . pack

fullPath :: String -> Collectd String
fullPath s = do
  collecdFile <- ask
  return . intercalate "/" $ ["/files", collecdFile, dropWhile (== '/') s]

appendArg :: ConfigPath -> ConfigArgument -> Collectd ()
appendArg s v = void (augSet' (s ++ "/arg[last()+1]") v)

type RootPath = Maybe ConfigPath

set :: RootPath -> ConfigStruct -> Collectd ()
set root (Directive n as) = do
  let root' = fromMaybe "" root
  p <- fullPath (root' ++ "directive[.=\"" ++ n ++ "\"]")
  augRm' p
  fullPath (root' ++ "directive[last()+1]") >>= flip augSet' n
  mapM_ (appendArg p) as
set root (Section n as cs) = do
  augRm' p
  mapM_ (appendArg p) as
  mapM_ (set (Just $ root' ++ "/" ++ n)) cs

-- getDirective :: RootPath -> Collectd (ConfigStruct)
-- getDirective

rm :: Maybe ConfigPath -> ConfigStruct -> Collectd ()
rm root (Directive n _) = do
  let root' = fromMaybe "" root
  p <- fullPath (root' ++ "directive[.=\"" ++ n ++ "\"]")
  void (augRm' p)
rm root (Section n _ _) = do
  let root' = fromMaybe "" root
  p <- fullPath (root' ++ "/" ++ n)
  void (augRm' p)

setGlobal :: ConfigStruct -> Collectd ()
setGlobal = set Nothing

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
  deriving (Eq, Show)

type Collectd a = ReaderT FilePath (EitherT CollectdError (StateT AugeasSession IO)) a

section :: String -> Collectd ByteString.ByteString
section name = do
  collecdFile <- ask
  return . ByteString.concat $ ["/files/", pack collecdFile, "/", pack name, "\"]"]

optFromMaybe :: Maybe a -> Optional a
optFromMaybe = maybe Default Specific

liftAugeas :: Augeas a -> Collectd a
liftAugeas a = ReaderT $ const (coerceAugeasFailure a AugeasFailure)

getMultiValueGlobal :: ([String] -> Maybe a) -> String -> Collectd (Optional a)
getMultiValueGlobal f name = do
  k2v <- liftAugeas $ augMatch
                        (ByteString.concat
                           ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/*"])
  let values = mapMaybe snd . fromMaybe [] $ k2v
  case f values of
    Nothing -> left' (ConfigParseError name (show values))
    r       -> return . optFromMaybe $ r

getSimpleGlobal :: (String -> Maybe a) -> String -> Collectd (Optional a)
getSimpleGlobal f name = do
  e <- liftAugeas $ augGet
                      (ByteString.concat
                         ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/arg"])
  case e of
    Nothing -> return Default
    Just value ->
      case f value of
        Nothing -> left' $ ConfigParseError name value
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

-- setDirective :: VarName -> Optional [String] -> Collectd ()
-- setDirective name (Specific values) =
--   liftAugeas $ augRm
--                  (ByteString.concat
--                     ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]"])
--   liftAugeas $ augSet "/files/etc/collectd.conf/directive[0]" (pack name)
--   -- [zip [1..] values
--   liftAugeas $ augSet
--                  (ByteString.concat
--                     ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]/arg[1]"])
--                  (pack value')
--   return ()
-- setSimpleGlobal p name Default = do
--   liftAugeas $ augRm
--                  (ByteString.concat
--                     ["/files/etc/collectd.conf/directive[.=\"", pack name, "\"]"])
--   return ()


-- globals2ConfigStructs :: Globals -> [ConfigStruct]
-- globals2configStruct Globals
--   { autoLoadPlugin = autoLoadPlugin'
--   , baseDir = baseDir'
--   , pidFile = pidFile'
--   , pluginDir = pluginDir'
--   , hostname = hostname'
--   , interval = interval'
--   , includePath = includePath'
--   , readThreads = readThreads'
--   , timeout = timeout'
--   , typesDB = typesDB'
--   , writeThreads = writeThreads'
--   , writeQueueLimitHigh = writeQueueLimitHigh'
--   , writeQueueLimitLow = writeQueueLimitLow'
--   } = [
--       , Directive "AutoLoadPlugin" [autoLoadPlugin']
--       , Directive "BaseDir" [baseDir']
--       , Directive "Hostname" [hostname']
--   -- includePath' <- getIncludePath
--       , Directive "Interval" interval'
--       , Directive "PIDFile" pidFile'
--       , Directive "PluginDir" pluginDir'
--       , Directive "ReadThreads" readThreads'
--       , Directive "Timeout" timeout'
--   -- typesDB' <- getMultiValueGlobal Just "TypesDB"
--   setInt "WriteQueueLimitHigh" writeQueueLimitHigh'
--   setInt "WriteQueueLimitLow" writeQueueLimitLow'
--   setInt "WriteThreads" writeThreads'


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

type CollectdFile = FilePath

runCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a, AugeasSession)
runCollectd a f c = runAugeasBase AugeasFailure a (runReaderT c f)

evalCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a)
evalCollectd a f c = (fst <$>) (runCollectd a f c)

execCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO AugeasSession
execCollectd a f c = (snd <$>) (runCollectd a f c)

updateGlobals :: (Globals -> Globals) -> Collectd ()
updateGlobals f = do
  globals <- getGlobals
  setGlobals . f $ globals
  liftAugeas augSave

-- directive :: String -> Collectd ByteString.ByteString
-- directive name = do
--   collecdFile <- ask
--   return . ByteString.concat $ ["/files/", pack collecdFile, "directive[.=\"", pack name, "\"]"]
-- 
-- directiveArg :: String -> Argument -> Collectd ByteString.ByteString
-- directiveArg name All = do
--   d <- directive name
--   return . ByteString.concat $ [d, "/*"]
-- directiveArg name (Arg i) = do
--   d <- directive name
--   return . ByteString.concat $ [d, "/arg[", pack . show $ i, "]"]

