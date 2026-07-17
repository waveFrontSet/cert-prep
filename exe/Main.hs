module Main where

import CertPrep.App
import CertPrep.CLI (parseCLIOpts)
import CertPrep.TUI (runApp)
import System.Directory (canonicalizePath)

main :: IO ()
main = do
    opts <- parseCLIOpts
    runApp' (AppEnv opts) $ do
        configPath <- resolveConfigPath
        config <- loadConfig configPath
        registerConfig' configPath (title config)
        canonPath <- liftIO $ canonicalizePath configPath
        earned <- loadEarnedTrophies' canonPath
        sampledNE <- sampleNonEmpty config
        settings <- loadSettings
        mExplainEnv <- resolveExplainEnv settings
        liftIO $ void $ runApp canonPath mExplainEnv sampledNE earned
