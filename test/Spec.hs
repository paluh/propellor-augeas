{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString (ByteString, hPut)
import qualified Data.ByteString.Char8 as Char8
import qualified Propellor.Property.Augeas as Augeas
import qualified Propellor.Property.Collectd as Collectd
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

withCollectdConfig :: ByteString -> Collectd.Collectd a -> IO (Either Collectd.CollectdError a)
withCollectdConfig collectdConfiguration action = withPreloadedTempFile
                                                    "propellor-collectd-testroot"
                                                    "etc/collectd.conf"
                                                    collectdConfiguration
                                                    (\root ->
                                                       Collectd.evalCollectd
                                                         (Augeas.defaultConfig
                                                            { Augeas.augeasRoot = Char8.pack root })
                                                         "/etc/collectd.conf"
                                                         action)

withCollectdModificationAndCheck :: ByteString -> Collectd.Collectd a -> Collectd.Collectd a -> IO (Either Collectd.CollectdError a)
withCollectdModificationAndCheck collectdConfiguration modification check = withPreloadedTempFile
                                                    "propellor-collectd-testroot"
                                                    "etc/collectd.conf"
                                                    collectdConfiguration
                                                    (\root -> do
                                                      (Collectd.evalCollectd (Augeas.defaultConfig { Augeas.augeasRoot = Char8.pack root }) "/etc/collectd.conf" modification)
                                                      Collectd.evalCollectd
                                                        (Augeas.defaultConfig
                                                          { Augeas.augeasRoot = Char8.pack root })
                                                          "/etc/collectd.conf"
                                                          check)



main :: IO ()
main = hspec $ do
  describe "Prollelor.Property.Collectd globals" $ do
    it "updates values correctly" $
      withCollectdConfig (Char8.pack collectdConf) (do
          Collectd.setGlobal (Collectd.Directive "test" ["value"])

        )
        `shouldReturn` (Right ())
  --     8 `shouldBe` 8
  -- describe "Prollelor.Property.Collectd globals" $ do
  --   it "updates values correctly" $ do
  --     8 `shouldBe` 8
