module Main where

import CLI (CLIOptions (..), parseCLIOpts)
import Control.Monad (void)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Registry (loadRegistry, registerConfig)
import Sampling (SamplingStrategy (..), sampleQuestions)
import System.Directory (canonicalizePath)
import System.Exit (exitFailure)
import System.Random (newStdGen)
import TUI (runApp, selectConfig)
import Trophy (loadEarnedTrophies)
import Types (Config (..))

main :: IO ()
main = do
    opts <- parseCLIOpts

    configPath <- case cliConfigPath opts of
        Just p -> pure p
        Nothing -> do
            registry <- loadRegistry
            if null registry
                then do
                    putStrLn "No config path provided and no previously-used configs found."
                    putStrLn "Usage: cert-prep <config.json>"
                    exitFailure
                else do
                    mPath <- selectConfig registry
                    maybe exitFailure pure mPath

    configBytes <- BS.readFile configPath
    config <- case eitherDecodeStrict configBytes of
        Left err -> do
            putStrLn $ "Error parsing config: " ++ err
            exitFailure
        Right c -> return c

    registerConfig configPath (title config)

    canonPath <- canonicalizePath configPath
    earned <- loadEarnedTrophies canonPath

    let sampleSize = fromMaybe (sampleAmount config) (cliSampleAmount opts)
        strategy = case cliWeights opts of
            [] -> maybe Uniform Stratified $ categoryWeights config
            ws -> Stratified (Map.fromList ws)
        allQuestions = questions config
        effectiveSize = min sampleSize (length allQuestions)

    gen <- newStdGen
    let sampledQuestions =
            sampleQuestions gen effectiveSize strategy allQuestions

    sampledQuestionsNE <- case NE.nonEmpty sampledQuestions of
        Nothing -> do
            putStrLn "No questions found in config"
            exitFailure
        Just ne -> pure ne

    void $ runApp sampledQuestionsNE canonPath earned
