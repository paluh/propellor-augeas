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

data Selector = Selector Label [Argument] (Maybe Index)

labelSelector :: Label -> Selector
labelSelector label = Selector label [] Nothing

data Node = Directive Label [Argument]
          | Section Label [Argument] [Node]
  deriving (Eq, Show)

data CollectdError = ConfigParseError Label Value
                   | AugeasFailure AugeasFailure
  deriving (Eq, Show)

type Collectd a = ReaderT FilePath (EitherT CollectdError (StateT AugeasSession IO)) a

left' :: CollectdError -> Collectd a
left' = ReaderT . const . left

liftAugeas :: Augeas a -> Collectd a
liftAugeas a = ReaderT $ const (coerceAugeasFailure a AugeasFailure)

collectdGet :: Path -> Collectd (Maybe String)
collectdGet path = fullPath path >>= liftAugeas . augGet . pack

collectdMatch :: Path -> Collectd (Maybe [(Path, Maybe String)])
collectdMatch path = fullPath path >>= liftAugeas . augMatch . pack

fullPath :: Path -> Collectd Path
fullPath s = do
  collecdFile <- ask
  return $ "/files" ++ collecdFile ++ "/" ++ dropWhile (== '/') s

sectionQuery :: Selector -> String
sectionQuery (Selector label arguments index) =
  label ++
  concatMap (\a -> "[arg=\"" ++ a ++ "\"]") arguments ++
  maybe "" (\i -> "[" ++ show i ++ "]") index
directiveQuery :: Selector -> String
directiveQuery (Selector label arguments index) =
  "directive[.=\"" ++ label ++ "\"]" ++
  concatMap (\a -> "[arg=\"" ++ a ++ "\"]") arguments ++
  maybe "" (\i -> "[" ++ show i ++ "]") index

getNodes :: Maybe Root -> Selector -> Collectd [Node]
getNodes root selector = (++) <$> getSections root selector
                              <*> getDirectives root selector

getSections :: Maybe Root -> Selector -> Collectd [Node]
getSections root selector@(Selector label args _) = do
  let root' = fromMaybe "" root
      sectionsPath = root' ++ sectionQuery selector
  mK2v <- collectdMatch sectionsPath
  case mK2v of
    Nothing -> return []
    Just [] -> return []
    Just k2v ->
      mapM (getSection root) . mapMaybe ((Selector label args . Just <$>)
                             . extractSectionIndex . fst) $ k2v
 where
  extractSectionIndex :: String -> Maybe Int
  extractSectionIndex label =
    case listToMaybe (label =~ (".*/" ++ label ++ "\\[([0-9]+)\\]$") :: [[String]]) of
      Nothing -> Just 1
      Just [w, s] -> readMaybe s

getSection :: Maybe Root -> Selector -> Collectd Node
getSection root selector@(Selector label args index) = do
  let root' = fromMaybe "" root
      path = root' ++ sectionQuery selector
  -- guard against missing node
  collectdGet path
  arguments <- maybe [] (mapMaybe snd) <$> collectdMatch (path ++ "/arg")
  (subdirectives, subsections) <- maybe ([], []) (partition (\(k, v) -> isJust v))
                                  <$> collectdMatch (path ++ "/*[label() != \"arg\"]")
  pathWithoutIndex <- fullPath (root' ++ label ++ "/")
  let subsectionsNames = mapMaybe (stripPrefix pathWithoutIndex . fst) subsections
  subnodes <- concat <$> ((++) <$> mapM (getDirectives . Just $ path ++ "/")
                                        [labelSelector d | (_, Just d) <- subdirectives]
                               <*> mapM ((getSections . Just $ path ++ "/") . labelSelector)
                                        subsectionsNames)
  return $ Section label arguments subnodes

getDirectives :: Maybe Root -> Selector -> Collectd [Node]
getDirectives root selector@(Selector label args index) = do
  let root' = fromMaybe "" root
      directivePath = root' ++ directiveQuery selector
  mK2v <- collectdMatch directivePath
  case mK2v of
    Nothing -> return []
    Just [] -> return []
    Just k2v ->
      mapM (getDirective root) . mapMaybe ((Selector label args . Just <$>)
                               . extractDirectiveIndex . fst) $ k2v
 where
  extractDirectiveIndex :: String -> Maybe Int
  extractDirectiveIndex label =
    case listToMaybe (label =~ (".*/directive\\[([0-9]+)\\]$" :: String) :: [[String]]) of
      Nothing -> Just 1
      Just [w, s] -> readMaybe s

getDirective :: Maybe Root -> Selector -> Collectd Node
getDirective root selector@(Selector label args index) = do
  let root' = fromMaybe "" root
      path = root' ++ directiveQuery selector
      argsPath = path ++ "/*"
  -- guard against missing node
  collectdGet path
  a2v <- fromMaybe [] <$> collectdMatch argsPath
  return $ Directive label . mapMaybe snd $ a2v

type CollectdFile = FilePath

runCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a, AugeasSession)
runCollectd a f c = runAugeasBase AugeasFailure a (runReaderT c f)

evalCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO (Either CollectdError a)
evalCollectd a f c = (fst <$>) (runCollectd a f c)

execCollectd :: AugeasConfig -> CollectdFile -> Collectd a -> IO AugeasSession
execCollectd a f c = (snd <$>) (runCollectd a f c)
