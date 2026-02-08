{-# LANGUAGE TemplateHaskell #-}

module State (
    Phase (..),
    Name (..),
    AppState (..),
    questions,
    currentIndex,
    selectedAnswers,
    focusedAnswer,
    phase,
    score,
    elapsedSeconds,
    currentQuestion,
    totalQuestions,
    initialState,
)
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lens.Micro.TH (makeLenses)
import Types (Question)

data Phase = Answering | Reviewing | Finished
    deriving (Show, Eq)

data Name
    = AnswerChoice Int
    | SubmitButton
    | NextButton
    deriving (Show, Eq, Ord)

data AppState = AppState
    { _questions :: Vector Question
    , _currentIndex :: Int
    , _selectedAnswers :: IntSet
    , _focusedAnswer :: Int
    , _phase :: Phase
    , _score :: Int
    , _elapsedSeconds :: Int
    }
    deriving (Show)

makeLenses ''AppState

currentQuestion :: AppState -> Maybe Question
currentQuestion s = _questions s V.!? _currentIndex s

totalQuestions :: AppState -> Int
totalQuestions s = V.length (_questions s)

initialState :: [Question] -> AppState
initialState qs =
    AppState
        { _questions = V.fromList qs
        , _currentIndex = 0
        , _selectedAnswers = IS.empty
        , _focusedAnswer = 0
        , _phase = Answering
        , _score = 0
        , _elapsedSeconds = 0
        }
