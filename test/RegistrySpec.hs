module RegistrySpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Registry
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

mkEntry :: Text -> FilePath -> UTCTime -> RegistryEntry
mkEntry title path lastUsed =
    RegistryEntry
        { registryEntryTitle = title
        , registryEntryPath = path
        , registryEntryLastUsed = lastUsed
        }

spec :: Spec
spec = do
    describe "RegistryEntry JSON" $ do
        it "roundtrips through JSON" $ do
            now <- getCurrentTime
            let entry = mkEntry "Test Config" "/tmp/test.json" now
            decode (encode entry) `shouldBe` Just entry

        it "roundtrips a list through JSON" $ do
            now <- getCurrentTime
            let entries =
                    [ mkEntry "Config A" "/a.json" now
                    , mkEntry "Config B" "/b.json" now
                    ]
            decode (encode entries) `shouldBe` Just entries

    describe "loadRegistry / saveRegistry" $ do
        it "returns [] when no registry file exists" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                registry <- loadRegistry
                registry `shouldBe` []

        it "roundtrips entries through save/load" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                now <- getCurrentTime
                let entries =
                        [ mkEntry "Config A" "/a.json" now
                        , mkEntry "Config B" "/b.json" now
                        ]
                saveRegistry entries
                loaded <- loadRegistry
                loaded `shouldBe` entries

    describe "registerConfig" $ do
        it "adds a new entry" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                -- Create a dummy file to canonicalize
                let configPath = tmpDir <> "/test.json"
                writeFile configPath "{}"
                registerConfig configPath "My Config"
                registry <- loadRegistry
                case registry of
                    [e] -> registryEntryTitle e `shouldBe` "My Config"
                    _ -> expectationFailure $ "Expected 1 entry, got " ++ show (length registry)

        it "upserts existing entry by path" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                let configPath = tmpDir <> "/test.json"
                writeFile configPath "{}"
                registerConfig configPath "Title v1"
                registerConfig configPath "Title v2"
                registry <- loadRegistry
                case registry of
                    [e] -> registryEntryTitle e `shouldBe` "Title v2"
                    _ -> expectationFailure $ "Expected 1 entry, got " ++ show (length registry)

        it "keeps entries sorted by lastUsed descending" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                let pathA = tmpDir <> "/a.json"
                    pathB = tmpDir <> "/b.json"
                writeFile pathA "{}"
                writeFile pathB "{}"
                registerConfig pathA "Config A"
                registerConfig pathB "Config B"
                registry <- loadRegistry
                case registry of
                    (e : _) -> registryEntryTitle e `shouldBe` "Config B"
                    [] -> expectationFailure "Expected non-empty registry"
