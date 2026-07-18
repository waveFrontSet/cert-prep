module CertPrep.Registry (
    RegistryEntry (..),
    Registry,
    registryFilePath,
    loadFile,
    loadRegistry,
    saveRegistry,
    registerConfig,
) where

import Data.Aeson (
    FromJSON,
    ToJSON,
    eitherDecodeStrict,
    encode,
 )
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (
    XdgDirectory (XdgConfig),
    canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (takeDirectory, (</>))

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
    pure $ dir </> "registry.json"

loadRegistry :: IO Registry
loadRegistry = do
    path <- registryFilePath
    fromRight [] <$> loadFile path

loadFile :: (FromJSON a) => FilePath -> IO (Either String a)
loadFile p = do
    exists <- doesFileExist p
    if not exists
        then return (Left $ "File does not exist: " <> show p)
        else do
            bytes <- readFileBS p
            return $ eitherDecodeStrict bytes

saveRegistry :: Registry -> IO ()
saveRegistry entries = do
    p <- registryFilePath
    createDirectoryIfMissing True (takeDirectory p)
    writeFileBS p (toStrict $ encode entries)

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
            sortWith (Down . lastUsed) $
                entry : filter (\e -> path e /= canonPath) existing
    saveRegistry updated
