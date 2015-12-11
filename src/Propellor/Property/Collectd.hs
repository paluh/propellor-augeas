{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Propellor.Property.Collectd where

import           Control.Monad (mapM_, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT(..), left, runEitherT)
import           Control.Monad.Trans.State.Strict (evalStateT, get, gets, put, modify, StateT(..), runStateT, state)

import           Control.Monad.Trans.Reader (ask, ReaderT(..), reader, runReaderT)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Char8 (pack, unpack)
import           Data.List (intercalate, partition, stripPrefix)
import           Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe, fromMaybe, maybe)
import           Data.Optional (Optional(..))
import           System.Augeas (AugRet)
import           Text.Regex.Posix ((=~))
import           Propellor.Property.Augeas (Augeas, AugeasBase, AugeasConfig, augGet, augMatch, augRm,
                                            augSave, augSet, AugeasFailure, AugeasSession,
                                            coerceAugeasFailure, runAugeasBase)
import           Text.Read (readMaybe)


-- From httpd.aug (collectd.aug uses the same parser):
--
--  Apache configuration is represented by two main nodeures, nested sections
--  and directives. Sections are used as labels, while directives are kept as a
--  value. Sections and directives can have positional arguments inside values
--  of "arg" nodes. Arguments of sections must be the firsts child of the
--  section node.

type Label = String

type Value = String

type Index = Int

type Path = String

type Root = Path

type Argument = String

-- minimal selector which is sufficient
-- in case of collectd configuration
data Selector = Selector (Maybe Root) Label [Argument] (Maybe Index)
  deriving (Eq, Show)

labelSelector :: Label -> Selector
labelSelector label = Selector Nothing label [] Nothing

data Node = Node Label [Argument] [Node]
  deriving (Eq, Show)

data CollectdError = ConfigParseError Label Value
                   | MissingNode Selector
                   | MultipleNodesMatching Selector
                   | AugeasFailure AugeasFailure
  deriving (Eq, Show)

type Collectd a = ReaderT FilePath (EitherT CollectdError (StateT AugeasSession IO)) a

type CollectdFile = FilePath

runCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a, AugeasSession)
runCollectd a f c = runAugeasBase AugeasFailure a (runReaderT c f)

evalCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a)
evalCollectd a f c = (fst <$>) (runCollectd a f c)

execCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO AugeasSession
execCollectd a f c = (snd <$>) (runCollectd a f c)

left' :: CollectdError -> Collectd a
left' = ReaderT . const . left

liftAugeas :: Augeas a -> Collectd a
liftAugeas a = ReaderT $ const (coerceAugeasFailure a AugeasFailure)

collectdGet :: Path -> Collectd (Maybe String)
collectdGet path = fullPath path >>= liftAugeas . augGet . pack

collectdMatch :: Path -> Collectd [(Path, Maybe String)]
collectdMatch path = fullPath path >>= liftAugeas . augMatch . pack

fullPath :: Path -> Collectd Path
fullPath s = do
  collecdFile <- ask
  return $ "/files" ++ collecdFile ++ "/" ++ dropWhile (== '/') s

sectionQuery :: Selector -> String
sectionQuery (Selector root label arguments index) =
  fromMaybe "" root ++
  label ++
  concatMap (\a -> "[arg=\"" ++ a ++ "\"]") arguments ++
  maybe "" (\i -> "[" ++ show i ++ "]") index
directiveQuery :: Selector -> String
directiveQuery (Selector root label arguments index) =
  fromMaybe "" root ++
  "directive[.=\"" ++ label ++ "\"]" ++
  concatMap (\a -> "[arg=\"" ++ a ++ "\"]") arguments ++
  maybe "" (\i -> "[" ++ show i ++ "]") index

getNodes :: Selector -> Collectd [Node]
getNodes selector = (++) <$> getSections selector
                         <*> getDirectives selector

getNode :: Selector -> Collectd Node
getNode selector = do
  o <- getOptionalNode selector
  case o of
    Specific n -> return n
    Default    -> left' . MissingNode $ selector

getOptionalNode :: Selector -> Collectd (Optional Node)
getOptionalNode selector = do
  nodes <- getNodes selector
  case nodes of
    [n]       -> return . Specific $ n
    []        -> return Default
    otherwise -> left' . MultipleNodesMatching $ selector

getSections :: Selector -> Collectd [Node]
getSections selector@(Selector root label args _) = do
  let sectionsPath = sectionQuery selector
  k2v <- collectdMatch sectionsPath
  mapM getSection . mapMaybe ((Selector root label args . Just <$>)
                  . extractSectionIndex . fst) $ k2v
 where
  extractSectionIndex :: String -> Maybe Int
  extractSectionIndex label =
    case listToMaybe (label =~ (".*/" ++ label ++ "\\[([0-9]+)\\]$") :: [[String]]) of
      Nothing -> Just 1
      Just [w, s] -> readMaybe s

getSection :: Selector -> Collectd Node
getSection selector@(Selector root label args index) = do
  let path = sectionQuery selector
  -- guard against missing node
  collectdGet path
  arguments <- mapMaybe snd <$> collectdMatch (path ++ "/arg")
  (subdirectives, subsections) <- partition (\(k, v) -> isJust v)
                                    <$> collectdMatch (path ++ "/*[label() != \"arg\"]")
  pathWithoutIndex <- (++ "/") <$> fullPath (sectionQuery (Selector root label args Nothing))
  let subsectionsNames = mapMaybe (stripPrefix pathWithoutIndex . fst) subsections
  subnodes <- concat <$> ((++) <$> mapM getDirectives
                                        [Selector (Just (path ++ "/")) d [] Nothing | (_, Just d) <- subdirectives]
                               <*> mapM (getSections . (\l -> Selector (Just (path ++ "/")) l [] Nothing))
                                        subsectionsNames)
  return $ Node label arguments subnodes

getDirectives :: Selector -> Collectd [Node]
getDirectives selector@(Selector root label args index) = do
  let directivePath = directiveQuery selector
  k2v <- collectdMatch directivePath
  mapM getDirective . mapMaybe ((Selector root label args . Just <$>)
                    . extractDirectiveIndex . fst) $ k2v
 where
  extractDirectiveIndex :: String -> Maybe Int
  extractDirectiveIndex label =
    case listToMaybe (label =~ (".*/directive\\[([0-9]+)\\]$" :: String) :: [[String]]) of
      Nothing -> Just 1
      Just [w, s] -> readMaybe s

getDirective :: Selector -> Collectd Node
getDirective selector@(Selector root label args index) = do
  let path = directiveQuery selector
      argsPath = path ++ "/*"
  -- guard against missing node
  collectdGet path
  a2v <- collectdMatch argsPath
  return $ Node label (mapMaybe snd a2v) []

type Seconds = Int
type Iterations = Int

type Filter = String
data Include =
  Include FilePath
          Filter
  deriving (Show)

data Globals =
       Globals
         { autoLoadPlugin :: Optional Bool
         , baseDir :: Optional FilePath
         , hostname :: Optional String
         , includePath :: Optional Include
         , interval :: Optional Seconds
         , pidFile :: Optional FilePath
         , pluginDir :: Optional FilePath
         , readThreads :: Optional Int
         , typesDB :: Optional [FilePath]
         , timeout :: Optional Iterations
         , writeThreads :: Optional Int
         , writeQueueLimitHigh :: Optional Int
         , writeQueueLimitLow :: Optional Int
         }
  -- PostCacheChain ChainName
  -- FQDNLookup true|false
  -- PreCacheChain ChainName
  deriving Show

-- getByLabel :: (String -> a) -> Label -> Collectd a
-- getByLabel parse label = do
--   node <- getNode (labelSelector label)
--   case node of
--     Directive _ [v] _ -> return . parse v
--     Selector _ _ 
--
-- getGlobals :: Collectd Globals
--   autoLoadPlugin' <- getNode "AutoLoadPlugin"
