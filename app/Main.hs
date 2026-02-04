module Main where

import Brick
import Control.Monad (void, when)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict, genericParseJSON)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (sortBy, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (RandomGen, newStdGen, uniformR)
import TUI.Attributes (theMap)
import TUI.Draw (drawUI)
import TUI.Event (handleEvent)
import TUI.State (AppState, Name, initialState)
import Types (Question (..))

data Config = Config
    { configQuestions :: [Question]
    , configSampleAmount :: Int
    }
    deriving (Show, Eq, Generic)

toLowerFirstLetter :: String -> String
toLowerFirstLetter [] = []
toLowerFirstLetter (x : xs) = toLower x : xs

configOptions :: Options
configOptions =
    defaultOptions
        { fieldLabelModifier = \s ->
            toLowerFirstLetter $ fromMaybe s (stripPrefix "config" s)
        }

instance FromJSON Config where
    parseJSON = genericParseJSON configOptions

sampleQuestions :: (RandomGen g) => g -> Int -> [Question] -> [Question]
sampleQuestions _ 0 _ = []
sampleQuestions gen n qs
    | n >= length qs = qs
    | otherwise = map snd $ sortBy (comparing fst) $ take n $ shuffle gen (zip [0 ..] qs)
  where
    shuffle :: (RandomGen g) => g -> [(Int, a)] -> [(Int, a)]
    shuffle g xs =
        let (shuffled, _) = foldl shuffleStep ([], g) xs
         in shuffled

    shuffleStep :: (RandomGen g) => ([(Int, a)], g) -> (Int, a) -> ([(Int, a)], g)
    shuffleStep (acc, g) x =
        let (r, g') = uniformR (0 :: Int, 1000000) g
         in ((r, snd x) : acc, g')

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
