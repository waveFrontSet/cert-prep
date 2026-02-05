module Main where

import Brick
import Control.Monad (void, when)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Sampling (sampleQuestions)
import State (AppState, Name, initialState)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (newStdGen)
import TUI.Attributes (theMap)
import TUI.Draw (drawUI)
import TUI.Event (handleEvent)
import Types (Config (..))

app :: App AppState e Name
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
    args <- getArgs
    configPath <- case args of
        [path] -> return path
        _ -> do
            putStrLn "Usage: cert-prep <config.json>"
            exitFailure

    configBytes <- BS.readFile configPath
    config <- case eitherDecodeStrict configBytes of
        Left err -> do
            putStrLn $ "Error parsing config: " ++ err
            exitFailure
        Right c -> return c

    gen <- newStdGen
    let allQuestions = configQuestions config
        sampleSize = min (configSampleAmount config) (length allQuestions)
        sampledQuestions = sampleQuestions gen sampleSize allQuestions

    when (null sampledQuestions) $ do
        putStrLn "No questions found in config"
        exitFailure

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app (initialState sampledQuestions)
