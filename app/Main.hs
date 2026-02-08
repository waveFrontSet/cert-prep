module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import CLI (CLIOptions (..), parseCLIOpts)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Registry (loadRegistry, registerConfig)
import Sampling (SamplingStrategy (..), sampleQuestions)
import State (AppState, Name, initialState)
import System.Exit (exitFailure)
import System.Random (newStdGen)
import TUI.Attributes (theMap)
import TUI.ConfigSelect (selectConfig)
import TUI.Draw (drawUI)
import TUI.Event (CustomEvent (..), handleEvent)
import Types (Config (..))

app :: App AppState CustomEvent Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

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

    registerConfig configPath (configTitle config)

    let sampleSize = fromMaybe (configSampleAmount config) (cliSampleAmount opts)
        strategy = case cliWeights opts of
            [] -> maybe Uniform Stratified $ configCategoryWeights config
            ws -> Stratified (Map.fromList ws)
        allQuestions = configQuestions config
        effectiveSize = min sampleSize (length allQuestions)

    gen <- newStdGen
    let sampledQuestions =
            sampleQuestions gen effectiveSize strategy allQuestions

    when (null sampledQuestions) $ do
        putStrLn "No questions found in config"
        exitFailure

    chan <- newBChan 10
    void $ forkIO $ forever $ do
        threadDelay 1000000
        writeBChan chan Tick

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    void $
        customMain
            initialVty
            buildVty
            (Just chan)
            app
            (initialState sampledQuestions)
