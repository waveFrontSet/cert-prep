{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad (void, when)
import Data.Aeson (FromJSON (..), Options (..), defaultOptions, eitherDecodeStrict, genericParseJSON)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (sortBy, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (RandomGen, newStdGen, uniformR)
import Types (Question (..), isCorrect)

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

data Phase = Answering | Reviewing | Finished
    deriving (Show, Eq)

data Name
    = AnswerChoice Int
    | SubmitButton
    | NextButton
    deriving (Show, Eq, Ord)

data AppState = AppState
    { _questions :: [Question]
    , _currentIndex :: Int
    , _selectedAnswers :: IntSet
    , _phase :: Phase
    , _score :: Int
    }
    deriving (Show)

makeLenses ''AppState

currentQuestion :: AppState -> Maybe Question
currentQuestion s =
    let idx = s ^. currentIndex
        qs = s ^. questions
     in if idx < length qs then Just (qs !! idx) else Nothing

totalQuestions :: AppState -> Int
totalQuestions s = length (s ^. questions)

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = case s ^. phase of
        Finished -> drawFinished s
        _ -> drawExam s

drawFinished :: AppState -> Widget Name
drawFinished s =
    withBorderStyle unicode $
        borderWithLabel (str " Exam Complete ") $
            center $
                vBox
                    [ str ""
                    , hCenter $ str "Your Results"
                    , str ""
                    , hCenter $ str $ "Score: " ++ show (s ^. score) ++ " / " ++ show (totalQuestions s)
                    , hCenter $ str $ "Percentage: " ++ show percentage ++ "%"
                    , str ""
                    , hCenter $ str "Press 'q' or Esc to exit"
                    , str ""
                    ]
  where
    percentage :: Int
    percentage =
        if totalQuestions s == 0
            then 0
            else (s ^. score * 100) `div` totalQuestions s

drawExam :: AppState -> Widget Name
drawExam s =
    vBox
        [ hBox
            [ questionPanel
            , vBorder
            , answersPanel
            ]
        , hBorder
        , statusBar
        ]
  where
    mQuestion = currentQuestion s

    questionPanel =
        withBorderStyle unicode $
            borderWithLabel (str $ " Question " ++ show (s ^. currentIndex + 1) ++ " of " ++ show (totalQuestions s) ++ " ") $
                hLimitPercent 50 $
                    padAll 1 $
                        case mQuestion of
                            Nothing -> str "No question"
                            Just q -> strWrap (questionText q)

    answersPanel =
        withBorderStyle unicode $
            borderWithLabel (str " Answers ") $
                hLimitPercent 50 $
                    padAll 1 $
                        case mQuestion of
                            Nothing -> str "No answers"
                            Just q -> vBox $ zipWith (drawAnswer s q) [0 ..] (questionAnswerChoices q)

    statusBar =
        padLeftRight 1 $
            hBox
                [ str $ "Score: " ++ show (s ^. score) ++ "/" ++ show (s ^. currentIndex)
                , fill ' '
                , case s ^. phase of
                    Answering ->
                        clickable SubmitButton $
                            withAttr submitAttr $
                                str " [Enter] Submit "
                    Reviewing ->
                        clickable NextButton $
                            withAttr nextAttr $
                                str " [Enter] Next "
                    Finished -> str ""
                , fill ' '
                , str "[q] Quit  [Space] Toggle  [Arrow Keys] Navigate"
                ]

drawAnswer :: AppState -> Question -> Int -> String -> Widget Name
drawAnswer s q idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            hBox [checkbox, str " ", answerWidget]
  where
    selected = IS.member idx (s ^. selectedAnswers)
    correct = IS.member idx (questionCorrectAnswer q)

    checkbox = case s ^. phase of
        Answering ->
            if selected
                then withAttr selectedAttr $ str "[X]"
                else str "[ ]"
        Reviewing ->
            if correct && selected
                then withAttr correctAttr $ str "[X]"
                else
                    if correct
                        then withAttr correctAttr $ str "[O]"
                        else
                            if selected
                                then withAttr wrongAttr $ str "[X]"
                                else str "[ ]"
        Finished -> str "[ ]"

    answerWidget = case s ^. phase of
        Reviewing ->
            if correct
                then withAttr correctAttr $ strWrap answerText
                else
                    if selected
                        then withAttr wrongAttr $ strWrap answerText
                        else strWrap answerText
        _ -> strWrap answerText

selectedAttr, correctAttr, wrongAttr, submitAttr, nextAttr :: AttrName
selectedAttr = attrName "selected"
correctAttr = attrName "correct"
wrongAttr = attrName "wrong"
submitAttr = attrName "submit"
nextAttr = attrName "next"

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (selectedAttr, V.white `on` V.blue)
        , (correctAttr, fg V.green `V.withStyle` V.bold)
        , (wrongAttr, fg V.red `V.withStyle` V.bold)
        , (submitAttr, V.black `on` V.yellow)
        , (nextAttr, V.black `on` V.cyan)
        ]

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case s ^. phase of
        Answering -> submitAnswer
        Reviewing -> nextQuestion
        Finished -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    s <- get
    when (s ^. phase == Answering) $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                let numAnswers = length (questionAnswerChoices q)
                    current = findFocusedAnswer s
                    idx = maybe 0 id current
                when (idx < numAnswers) $ toggleAnswer idx
            Nothing -> return ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) = moveFocus (-1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = moveFocus 1
handleEvent (MouseDown (AnswerChoice idx) _ _ _) = do
    s <- get
    when (s ^. phase == Answering) $ toggleAnswer idx
handleEvent (MouseDown SubmitButton _ _ _) = do
    s <- get
    when (s ^. phase == Answering) submitAnswer
handleEvent (MouseDown NextButton _ _ _) = do
    s <- get
    when (s ^. phase == Reviewing) nextQuestion
handleEvent _ = return ()

findFocusedAnswer :: AppState -> Maybe Int
findFocusedAnswer s =
    let sel = s ^. selectedAnswers
     in if IS.null sel then Just 0 else Just (IS.findMin sel)

toggleAnswer :: Int -> EventM Name AppState ()
toggleAnswer idx = do
    sel <- use selectedAnswers
    if IS.member idx sel
        then selectedAnswers .= IS.delete idx sel
        else selectedAnswers .= IS.insert idx sel

moveFocus :: Int -> EventM Name AppState ()
moveFocus delta = do
    s <- get
    when (s ^. phase == Answering) $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                let numAnswers = length (questionAnswerChoices q)
                    sel = s ^. selectedAnswers
                    current = if IS.null sel then 0 else IS.findMin sel
                    new = (current + delta) `mod` numAnswers
                selectedAnswers .= IS.singleton new
            Nothing -> return ()

submitAnswer :: EventM Name AppState ()
submitAnswer = do
    s <- get
    mQ <- gets currentQuestion
    case mQ of
        Just q -> do
            let userAnswer = s ^. selectedAnswers
            when (isCorrect q userAnswer) $ score += 1
            phase .= Reviewing
        Nothing -> return ()

nextQuestion :: EventM Name AppState ()
nextQuestion = do
    s <- get
    let nextIdx = s ^. currentIndex + 1
    if nextIdx >= totalQuestions s
        then phase .= Finished
        else do
            currentIndex .= nextIdx
            selectedAnswers .= IS.empty
            phase .= Answering

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

initialState :: [Question] -> AppState
initialState qs =
    AppState
        { _questions = qs
        , _currentIndex = 0
        , _selectedAnswers = IS.empty
        , _phase = Answering
        , _score = 0
        }

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
