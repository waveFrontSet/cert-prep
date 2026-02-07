module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Applicative (many, optional)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Options.Applicative qualified as Opt
import Sampling (SamplingStrategy (..), sampleQuestions)
import State (AppState, Name, initialState)
import System.Exit (exitFailure)
import System.Random (newStdGen)
import TUI.Attributes (theMap)
import TUI.Draw (drawUI)
import TUI.Event (CustomEvent (..), handleEvent)
import Types (Config (..))

data CLIOptions = CLIOptions
    { cliSampleAmount :: Maybe Int
    , cliWeights :: [(String, Int)]
    , cliConfigPath :: FilePath
    }

cliParser :: Opt.Parser CLIOptions
cliParser =
    CLIOptions
        <$> optional
            ( Opt.option
                Opt.auto
                ( Opt.short 'n'
                    <> Opt.long "sample-amount"
                    <> Opt.metavar "N"
                    <> Opt.help "Number of questions to sample"
                )
            )
        <*> many
            ( Opt.option
                parseWeight
                ( Opt.short 'w'
                    <> Opt.long "weight"
                    <> Opt.metavar "CATEGORY:WEIGHT"
                    <> Opt.help
                        "Category weight (repeatable), e.g. \"AWS Storage:2\""
                )
            )
        <*> Opt.argument Opt.str (Opt.metavar "<config.json>")

parseWeight :: Opt.ReadM (String, Int)
parseWeight = Opt.eitherReader $ \s ->
    case break (== ':') (reverse s) of
        (revW, _ : revCat)
            | not (null revW)
            , not (null revCat)
            , [(w, "")] <- reads (reverse revW) ->
                Right (reverse revCat, w)
        _ -> Left $ "Invalid weight format: " ++ s ++ " (expected CATEGORY:WEIGHT)"

cliInfo :: Opt.ParserInfo CLIOptions
cliInfo =
    Opt.info
        (cliParser Opt.<**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Certification exam prep TUI"
            <> Opt.header "cert-prep - practice certification questions"
        )

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
    opts <- Opt.execParser cliInfo

    configBytes <- BS.readFile (cliConfigPath opts)
    config <- case eitherDecodeStrict configBytes of
        Left err -> do
            putStrLn $ "Error parsing config: " ++ err
            exitFailure
        Right c -> return c

    let sampleSize = fromMaybe (configSampleAmount config) (cliSampleAmount opts)
        strategy = case cliWeights opts of
            [] -> maybe Uniform Stratified $ configCategoryWeights config
            ws -> Stratified (Map.fromList [(T.pack k, v) | (k, v) <- ws])
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
