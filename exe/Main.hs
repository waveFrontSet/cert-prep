module Main where

import CertPrep.App (
    AppEnv (..),
    loadConfig,
    loadSettings,
    resolveConfigPath,
    resolveExplainEnv,
    runApp',
    sampleNonEmpty,
 )
import CertPrep.CLI (parseCLIOpts)
import CertPrep.Registry (registerConfig)
import CertPrep.TUI (runApp)
import CertPrep.Trophy (loadEarnedTrophies)
import CertPrep.Types (Config (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (canonicalizePath)

main :: IO ()
main = do
    opts <- parseCLIOpts
    runApp' (AppEnv opts) $ do
        configPath <- resolveConfigPath
        config <- loadConfig configPath
        liftIO $ registerConfig configPath (title config)
        canonPath <- liftIO $ canonicalizePath configPath
        earned <- liftIO $ loadEarnedTrophies canonPath
        sampledNE <- sampleNonEmpty config
        settings <- loadSettings
        mExplainEnv <- resolveExplainEnv settings
        liftIO $ void $ runApp canonPath mExplainEnv sampledNE earned
