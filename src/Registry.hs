module Registry (
    RegistryEntry (..),
    Registry,
    registryFilePath,
    loadRegistry,
    saveRegistry,
    registerConfig,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    eitherDecodeStrict,
    genericParseJSON,
    genericToJSON,
 )
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (
    XdgDirectory (XdgConfig),
    canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (takeDirectory)

import Data.Aeson qualified as Aeson

data RegistryEntry = RegistryEntry
    { registryEntryTitle :: Text
    , registryEntryPath :: FilePath
    , registryEntryLastUsed :: UTCTime
    }
    deriving (Show, Eq, Generic)

type Registry = [RegistryEntry]

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs

stripPrefix' :: String -> String -> String
stripPrefix' prefix s =
    toLowerFirstLetter $ fromMaybe s (stripPrefix prefix s)
  where
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (p : ps) (y : ys)
        | p == y = stripPrefix ps ys
        | otherwise = Nothing

registryEntryOptions :: Options
registryEntryOptions =
    defaultOptions
        { fieldLabelModifier = stripPrefix' "registryEntry"
        }

instance FromJSON RegistryEntry where
    parseJSON = genericParseJSON registryEntryOptions
instance ToJSON RegistryEntry where
    toJSON = genericToJSON registryEntryOptions

registryFilePath :: IO FilePath
registryFilePath = do
    dir <- getXdgDirectory XdgConfig "cert-prep"
    pure $ dir <> "/registry.json"

loadRegistry :: IO Registry
loadRegistry = do
    path <- registryFilePath
    exists <- doesFileExist path
    if not exists
        then pure []
        else do
            bytes <- BS.readFile path
            case eitherDecodeStrict bytes of
                Left _ -> pure []
                Right entries -> pure entries

saveRegistry :: Registry -> IO ()
saveRegistry entries = do
    path <- registryFilePath
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path (BS.toStrict $ Aeson.encode entries)

registerConfig :: FilePath -> Text -> IO ()
registerConfig path title = do
    canonPath <- canonicalizePath path
    now <- getCurrentTime
    existing <- loadRegistry
    let entry =
            RegistryEntry
                { registryEntryTitle = title
                , registryEntryPath = canonPath
                , registryEntryLastUsed = now
                }
        updated =
            sortBy (comparing (Down . registryEntryLastUsed)) $
                entry : filter (\e -> registryEntryPath e /= canonPath) existing
    saveRegistry updated
