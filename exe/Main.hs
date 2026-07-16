module Main where

import App (
    AppEnv (..),
    loadConfig,
    loadSettings,
    resolveConfigPath,
    resolveExplainEnv,
    runApp',
    sampleNonEmpty,
 )
import CLI (parseCLIOpts)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Registry (registerConfig)
import System.Directory (canonicalizePath)
import TUI (runApp)
import Trophy (loadEarnedTrophies)
import Types (Config (..))

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
