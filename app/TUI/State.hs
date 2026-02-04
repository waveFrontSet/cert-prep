{-# LANGUAGE TemplateHaskell #-}

module TUI.State
    ( Phase (..)
    , Name (..)
    , AppState (..)
    , questions
    , currentIndex
    , selectedAnswers
    , focusedAnswer
    , phase
    , score
    , currentQuestion
    , totalQuestions
    , initialState
    )
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
    { _questions :: [Question]
    , _currentIndex :: Int
    , _selectedAnswers :: IntSet
    , _focusedAnswer :: Int
    , _phase :: Phase
    , _score :: Int
    }
    deriving (Show)

makeLenses ''AppState

currentQuestion :: AppState -> Maybe Question
currentQuestion s =
    let idx = _currentIndex s
        qs = _questions s
     in if idx < length qs then Just (qs !! idx) else Nothing

totalQuestions :: AppState -> Int
totalQuestions s = length (_questions s)

initialState :: [Question] -> AppState
initialState qs =
    AppState
        { _questions = qs
        , _currentIndex = 0
        , _selectedAnswers = IS.empty
        , _focusedAnswer = 0
        , _phase = Answering
        , _score = 0
        }
