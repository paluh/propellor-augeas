{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString, hPut)
import qualified Data.ByteString.Char8 as Char8
import qualified Propellor.Property.Augeas as Augeas
import qualified Propellor.Property.Collectd as Collectd
import           Propellor.Property.Collectd (Node(..))
import           Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)
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
          directoryPath = joinPath [root, (dropWhile (== '/') dirs)]
          filePath = joinPath [directoryPath, filename]
      createDirectoryIfMissing True directoryPath
      -- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
      withFile
        filePath
        WriteMode
        (\handle -> hPut handle content >> hFlush handle >> action root)

collectdConf = unlines ["AutoLoadPlugin true"
                       , "Interval 1"]

withCollectdConfig :: String -> Collectd.Collectd a -> IO (Either Collectd.CollectdError a)
withCollectdConfig collectdConfiguration action = withPreloadedTempFile
                                                    "propellor-collectd-testroot"
                                                    "etc/collectd.conf"
                                                    (Char8.pack collectdConfiguration)
                                                    (\root ->
                                                       Collectd.evalCollectd
                                                         (Augeas.defaultConfig
                                                            { Augeas.augeasRoot = Char8.pack root })
                                                         "/etc/collectd.conf"
                                                         action)

withCollectdModificationAndCheck :: String -> Collectd.Collectd a -> Collectd.Collectd a -> IO (Either Collectd.CollectdError a)
withCollectdModificationAndCheck collectdConfiguration modification check = withPreloadedTempFile
                                                    "propellor-collectd-testroot"
                                                    "etc/collectd.conf"
                                                    (Char8.pack collectdConfiguration)
                                                    (\root -> do
                                                      (Collectd.evalCollectd (Augeas.defaultConfig { Augeas.augeasRoot = Char8.pack root }) "/etc/collectd.conf" modification)
                                                      Collectd.evalCollectd
                                                        (Augeas.defaultConfig
                                                          { Augeas.augeasRoot = Char8.pack root })
                                                          "/etc/collectd.conf"
                                                          check)



main :: IO ()
main = hspec $ do
  describe "Prollelor.Property.Collectd getSection" $ do
    it "returns Nothing for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (Collectd.getSection Nothing "BaseDir" 1) `shouldReturn`
          (Left . Collectd.AugeasFailure $ Augeas.NoMatch "/files/etc/collectd.conf/BaseDir[1]")
  describe "Prollelor.Property.Collectd getNodes" $ do
    it "returns empty set for non existing node" $
      withCollectdConfig "AutoLoadPlugin true"
        (Collectd.getNodes Nothing "BaseDir") `shouldReturn`
          (Right [])
    it "fetches section and directive with the same name" $
      withCollectdConfig (unlines ["AutoLoadPlugin true", "<AutoLoadPlugin true>", "</AutoLoadPlugin>"])
        (Collectd.getNodes Nothing "AutoLoadPlugin") `shouldReturn`
          (Right [Section "AutoLoadPlugin" ["true"] [], Directive "AutoLoadPlugin" $ ["true"]])
    it "fetches simple directive" $
      withCollectdConfig "AutoLoadPlugin true"
        (Collectd.getNodes Nothing "AutoLoadPlugin") `shouldReturn`
          (Right [Directive "AutoLoadPlugin" $ ["true"]])
    it "fetches simple section" $
      withCollectdConfig "<Plugin df>\n</Plugin>"
        (Collectd.getNodes Nothing "Plugin") `shouldReturn`
          (Right [(Section "Plugin" ["df"] [])])
    it "fetches section with subdirective" $
      withCollectdConfig
        (unlines
           ["<Plugin df>", "ValuesPercentage true", "</Plugin>"])
        (Collectd.getNodes Nothing "Plugin") `shouldReturn`
          (Right [(Section "Plugin" ["df"] [Directive "ValuesPercentage" ["true"]])])
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
        (Collectd.getSections Nothing "Plugin") `shouldReturn`
          (Right [Section "Plugin" ["df"] [ Directive "ValuesPercentage" ["true"]
                                          , Section "Subsection" ["8"]
                                              [Directive "Subdirective" ["value"]]]])
  -- describe "Propellor.Property.Collectd setNode" $ do
    
