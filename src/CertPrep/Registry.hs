module CertPrep.Registry (
    RegistryEntry (..),
    Registry,
    registryFilePath,
    loadRegistry,
    saveRegistry,
    registerConfig,
    toSortedList,
) where

import CertPrep.Common (
    canonicalizePath,
    configDir,
    createDirectoryIfMissing,
    loadFileAsMonoid,
 )
import Data.Aeson (
    FromJSON,
    ToJSON,
    encode,
 )
import Data.Map qualified as M
import Data.Time (UTCTime, getCurrentTime)
import System.FilePath (takeDirectory, (</>))

data RegistryEntry = RegistryEntry
    { title :: Text
    , path :: FilePath
    , lastUsed :: UTCTime
    }
    deriving (Show, Eq, Generic)

type Registry = Map FilePath RegistryEntry

instance FromJSON RegistryEntry
instance ToJSON RegistryEntry

registryFilePath :: (MonadIO m) => m FilePath
registryFilePath = (</> "registry.json") <$> configDir

loadRegistry :: (MonadIO m) => m Registry
loadRegistry = loadFileAsMonoid =<< registryFilePath

saveRegistry :: (MonadIO m) => Registry -> m ()
saveRegistry entries = do
    p <- registryFilePath
    createDirectoryIfMissing True (takeDirectory p)
    writeFileBS p (toStrict $ encode entries)

registerConfig :: (MonadIO m) => FilePath -> Text -> m ()
registerConfig p title = do
    canonPath <- canonicalizePath p
    now <- liftIO getCurrentTime
    existing <- loadRegistry
    let entry =
            RegistryEntry
                { title = title
                , path = canonPath
                , lastUsed = now
                }
        updated = M.insert canonPath entry existing
    saveRegistry updated

toSortedList :: Registry -> [RegistryEntry]
toSortedList = sortOn (Down . lastUsed) . toList
