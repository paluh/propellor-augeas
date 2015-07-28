{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString, hPut)
import qualified Data.ByteString.Char8 as Char8
import qualified Propellor.Property.Augeas as Augeas
import           Propellor.Property.Collectd (Collectd, CollectdError(..), evalCollectd, getSections,
                                              getNodes, getSection, labelSelector, Node(..),
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
  describe "Prollelor.Property.Collectd getSection" $
    it "returns Nothing for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (getSection Nothing (Selector "BaseDir" [] (Just 1))) `shouldReturn`
          (Left . AugeasFailure $ Augeas.NoMatch "/files/etc/collectd.conf/BaseDir[1]")
  describe "Prollelor.Property.Collectd getNodes" $ do
    it "returns empty set for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (getNodes Nothing (labelSelector "BaseDir")) `shouldReturn`
          Right []
    it "fetches section and directive with the same name" $
      withCollectdConfig (unlines ["AutoLoadPlugin true", "<AutoLoadPlugin true>", "</AutoLoadPlugin>"])
        (getNodes Nothing (labelSelector "AutoLoadPlugin")) `shouldReturn`
          Right [Section "AutoLoadPlugin" ["true"] [], Directive "AutoLoadPlugin" ["true"]]
    it "fetches simple directive" $
      withCollectdConfig "AutoLoadPlugin true"
        (getNodes Nothing (labelSelector "AutoLoadPlugin")) `shouldReturn`
          Right [Directive "AutoLoadPlugin" ["true"]]
    it "fetches simple section" $
      withCollectdConfig "<Plugin df>\n</Plugin>"
        (getNodes Nothing (labelSelector "Plugin")) `shouldReturn`
          Right [Section "Plugin" ["df"] []]
    it "fetches section with subdirective" $
      withCollectdConfig
        (unlines
           ["<Plugin df>", "ValuesPercentage true", "</Plugin>"])
        (getNodes Nothing (labelSelector "Plugin")) `shouldReturn`
          Right [Section "Plugin" ["df"] [Directive "ValuesPercentage" ["true"]]]
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
        (getSections Nothing (labelSelector "Plugin")) `shouldReturn`
          Right [Section "Plugin" ["df"] [ Directive "ValuesPercentage" ["true"]
                                         , Section "Subsection" ["8"]
                                            [Directive "Subdirective" ["value"]]]]

    it "fetches all matching nodes" $
      withCollectdConfig
        (unlines
          [ "LoadPlugin cpu"
          , "LoadPlugin load"
          , "<LoadPlugin df>"
          ,   "Interval 3600"
          , "</LoadPlugin>"
          ])
        (getNodes Nothing (labelSelector "LoadPlugin")) `shouldReturn`
          Right [Section "LoadPlugin" ["df"] [Directive "Interval" ["3600"]],Directive "LoadPlugin" ["cpu"],Directive "LoadPlugin" ["load"]]
    it "filters nodes by arguments" $
      withCollectdConfig
        (unlines
          [ "LoadPlugin cpu"
          , "LoadPlugin load"
          , "<LoadPlugin df>"
          ,   "Interval 3600"
          , "</LoadPlugin>"
          ])
        (getNodes Nothing (Selector "LoadPlugin" ["df"] Nothing)) `shouldReturn`
          Right [Section "LoadPlugin" ["df"] [Directive "Interval" ["3600"]]]
