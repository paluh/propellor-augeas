{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString, hPut)
import qualified Data.ByteString.Char8 as Char8
import qualified Propellor.Property.Augeas as Augeas
import           Propellor.Property.Collectd (Collectd, CollectdError(..), evalCollectd, getNodes,
                                              getNodes, getNode, labelSelector, Node(..),
                                              Selector(..))
-- import qualified Propellor.Property.Collectd as Collectd
import           Test.Hspec (describe, hspec, it, shouldReturn)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Posix (joinPath, splitFileName)
import           System.IO (hFlush, IOMode(..), withFile)
import           System.IO.Temp (withSystemTempDirectory)


withPreloadedTempFile :: String -> String -> ByteString -> (FilePath -> IO a) -> IO a
withPreloadedTempFile dirNameTemplate subpath content action =
  withSystemTempDirectory dirNameTemplate callback
  where
    callback root = do
      let (dirs, filename) = splitFileName subpath
          directoryPath = joinPath [root, dropWhile (== '/') dirs]
          filePath = joinPath [directoryPath, filename]
      createDirectoryIfMissing True directoryPath
      -- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
      withFile
        filePath
        WriteMode
        (\handle -> hPut handle content >> hFlush handle >> action root)

withCollectdConfig :: String -> Collectd a -> IO (Either CollectdError a)
withCollectdConfig collectdConfiguration action = withPreloadedTempFile
                                                    "propellor-collectd-testroot"
                                                    "etc/collectd.conf"
                                                    (Char8.pack collectdConfiguration)
                                                    (\root ->
                                                       evalCollectd
                                                         (Augeas.defaultConfig
                                                            { Augeas.augeasRoot = Char8.pack root })
                                                         "/etc/collectd.conf"
                                                         action)

-- withCollectdModificationAndCheck :: String -> Collectd.Collectd a -> Collectd.Collectd a -> IO (Either Collectd.CollectdError a)
-- withCollectdModificationAndCheck collectdConfiguration modification check = withPreloadedTempFile
--                                                     "propellor-collectd-testroot"
--                                                     "etc/collectd.conf"
--                                                     (Char8.pack collectdConfiguration)
--                                                     (\root -> do
--                                                       (Collectd.evalCollectd (Augeas.defaultConfig { Augeas.augeasRoot = Char8.pack root }) "/etc/collectd.conf" modification)
--                                                       Collectd.evalCollectd
--                                                         (Augeas.defaultConfig
--                                                           { Augeas.augeasRoot = Char8.pack root })
--                                                           "/etc/collectd.conf"
--                                                           check)



main :: IO ()
main = hspec $ do
  describe "Prollelor.Property.Collectd getNode" $
    it "returns Nothing for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (getNode (Selector Nothing "BaseDir" [] (Just 1))) `shouldReturn`
          (Left . AugeasFailure $ Augeas.NoMatch "/files/etc/collectd.conf/BaseDir[1]")
  describe "Prollelor.Property.Collectd getNodes" $ do
    it "returns empty set for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (getNodes (labelSelector "BaseDir")) `shouldReturn`
          Right []
    it "fetches section and directive with the same name" $
      withCollectdConfig (unlines ["AutoLoadPlugin true", "<AutoLoadPlugin true>", "</AutoLoadPlugin>"])
        (getNodes (labelSelector "AutoLoadPlugin")) `shouldReturn`
          Right [Node "AutoLoadPlugin" ["true"] [], Node "AutoLoadPlugin" ["true"] []]
    it "fetches simple directive" $
      withCollectdConfig "AutoLoadPlugin true"
        (getNodes (labelSelector "AutoLoadPlugin")) `shouldReturn`
          Right [Node "AutoLoadPlugin" ["true"] []]
    it "fetches simple section" $
      withCollectdConfig "<Plugin df>\n</Plugin>"
        (getNodes (labelSelector "Plugin")) `shouldReturn`
          Right [Node "Plugin" ["df"] []]
    it "fetches section with subdirective" $
      withCollectdConfig
        (unlines
           ["<Plugin df>", "ValuesPercentage true", "</Plugin>"])
        (getNodes (labelSelector "Plugin")) `shouldReturn`
          Right [Node "Plugin" ["df"] [Node "ValuesPercentage" ["true"] []]]
    -- I'm not sure if this config is even correct
    it "fetches section with subsection" $
      withCollectdConfig
        (unlines
           ["<Plugin df>",
              "<Subsection 8>",
                "Subdirective value",
              "</Subsection>",
              "ValuesPercentage true",
            "</Plugin>"])
        (getNodes (labelSelector "Plugin")) `shouldReturn`
          Right [Node "Plugin" ["df"] [ Node "ValuesPercentage" ["true"] []
                                         , Node "Subsection" ["8"]
                                            [Node "Subdirective" ["value"] []]]]

    it "fetches all matching nodes" $
      withCollectdConfig
        (unlines
          [ "LoadPlugin cpu"
          , "LoadPlugin load"
          , "<LoadPlugin df>"
          ,   "Interval 3600"
          , "</LoadPlugin>"
          ])
        (getNodes (labelSelector "LoadPlugin")) `shouldReturn`
          Right [ Node "LoadPlugin" ["df"] [Node "Interval" ["3600"] []]
                , Node "LoadPlugin" ["cpu"] []
                , Node "LoadPlugin" ["load"] []]
    it "filters nodes by arguments" $
      withCollectdConfig
        (unlines
          [ "LoadPlugin cpu"
          , "LoadPlugin load"
          , "<LoadPlugin df>"
          ,   "Interval 3600"
          , "</LoadPlugin>"
          ])
        (getNodes (Selector Nothing "LoadPlugin" ["df"] Nothing)) `shouldReturn`
          Right [Node "LoadPlugin" ["df"] [Node "Interval" ["3600"] []]]
