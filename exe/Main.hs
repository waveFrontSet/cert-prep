module Main where

import CertPrep.App
import CertPrep.CLI (parseCLIOpts)
import CertPrep.Common (canonicalizePath)
import CertPrep.TUI (runApp)

main :: IO ()
main = do
    opts <- parseCLIOpts
    runApp' (AppEnv opts) $ do
        configPath <- resolveConfigPath
        config <- loadConfig configPath
        registerConfig configPath (title config)
        canonPath <- canonicalizePath configPath
        earned <- loadEarnedTrophies canonPath
        sampledNE <- sampleNonEmpty config
        settings <- loadSettings
        mExplainEnv <- resolveExplainEnv settings
        void $ runApp canonPath mExplainEnv sampledNE earned
