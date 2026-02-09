module Registry (
    RegistryEntry (..),
    Registry,
    registryFilePath,
    loadRegistry,
    saveRegistry,
    registerConfig,
) where

import Data.Aeson (
    FromJSON,
    ToJSON,
    eitherDecodeStrict,
 )
import Data.ByteString qualified as BS
import Data.List (sortBy)
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
    { title :: Text
    , path :: FilePath
    , lastUsed :: UTCTime
    }
    deriving (Show, Eq, Generic)

type Registry = [RegistryEntry]

instance FromJSON RegistryEntry
instance ToJSON RegistryEntry

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
    p <- registryFilePath
    createDirectoryIfMissing True (takeDirectory p)
    BS.writeFile p (BS.toStrict $ Aeson.encode entries)

registerConfig :: FilePath -> Text -> IO ()
registerConfig p title = do
    canonPath <- canonicalizePath p
    now <- getCurrentTime
    existing <- loadRegistry
    let entry =
            RegistryEntry
                { title = title
                , path = canonPath
                , lastUsed = now
                }
        updated =
            sortBy (comparing (Down . lastUsed)) $
                entry : filter (\e -> path e /= canonPath) existing
    saveRegistry updated
