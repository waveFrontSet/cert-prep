module CertPrep.Common where

import Data.Aeson (
    FromJSON,
    eitherDecodeStrict,
 )
import System.Directory (
    XdgDirectory (..),
    getXdgDirectory,
 )
import System.Directory qualified as D (
    canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
 )

-- | The config directory for this application
configDir :: (MonadIO m) => m FilePath
configDir = liftIO $ getXdgDirectory XdgConfig "cert-prep"

-- | Lifted version of System.Directory.canonicalizePath
canonicalizePath :: (MonadIO m) => FilePath -> m FilePath
canonicalizePath = liftIO . D.canonicalizePath

-- | Lifted version of System.Directory.createDirectoryIfMissing
createDirectoryIfMissing :: (MonadIO m) => Bool -> FilePath -> m ()
createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b

-- | Lifted version of System.Directory.doesFileExist
doesFileExist :: (MonadIO m) => FilePath -> m Bool
doesFileExist = liftIO . D.doesFileExist

{- | Load and deserialize a JSON file, returning a default value
if it doesn't exist.
-}
loadFileWithDefault ::
    (FromJSON a, MonadIO m) => Maybe a -> FilePath -> m (Either String a)
loadFileWithDefault def p = do
    exists <- doesFileExist p
    if not exists
        then return $ maybeToRight ("File does not exist: " <> show p) def
        else eitherDecodeStrict <$> readFileBS p

-- | Load and deserialize a JSON file
loadFile :: (FromJSON a, MonadIO m) => FilePath -> m (Either String a)
loadFile = loadFileWithDefault Nothing

{- | Load and deserialize a JSON file as a monoid,
returning the `mempty` value in case of failure.
-}
loadFileAsMonoid :: (FromJSON a, Monoid a, MonadIO m) => FilePath -> m a
loadFileAsMonoid p = fromRight mempty <$> loadFile p

-- | Format a number of seconds as a time string
formatTime :: Int -> Text
formatTime totalSecs =
    pad mins <> ":" <> pad secs
  where
    mins = totalSecs `div` 60
    secs = totalSecs `mod` 60
    pad n = if n < 10 then "0" <> show n else show n
