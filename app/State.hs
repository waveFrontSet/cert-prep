{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module State (
    Name (..),
    ExamCore (..),
    AnsweringData (..),
    ReviewingData (..),
    ActivePhase (..),
    FinishedState (..),
    ExamPhase (..),
    questions,
    currentIndex,
    score,
    elapsedSeconds,
    selectedAnswers,
    focusedAnswer,
    activeCore,
    activeQuestion,
    phaseData,
    answerResult,
    lastSelected,
    finalScore,
    finalTotal,
    finalElapsed,
    overActiveCore,
    finishExam,
    totalQuestions,
    initialState,
)
where

import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lens.Micro ((%~), (&), (^.))
import Lens.Micro.TH (makeLenses)
import Types (AnswerResult, Question)

data Name
    = AnswerChoice Int
    | SubmitButton
    | NextButton
    deriving (Show, Eq, Ord)

data ExamCore = ExamCore
    { _questions :: Vector Question
    , _currentIndex :: Int
    , _score :: Int
    , _elapsedSeconds :: Int
    }
    deriving (Show)

makeLenses ''ExamCore

data AnsweringData = AnsweringData
    { _selectedAnswers :: IntSet
    , _focusedAnswer :: Int
    }
    deriving (Show)

makeLenses ''AnsweringData

data ReviewingData = ReviewingData
    { _answerResult :: AnswerResult
    , _lastSelected :: IntSet
    }
    deriving (Show)

makeLenses ''ReviewingData

data ActivePhase a = ActivePhase
    { _activeCore :: ExamCore
    , _activeQuestion :: Question
    , _phaseData :: a
    }
    deriving (Show)

makeLenses ''ActivePhase

data FinishedState = FinishedState
    { _finalScore :: Int
    , _finalTotal :: Int
    , _finalElapsed :: Int
    }
    deriving (Show)

makeLenses ''FinishedState

data ExamPhase
    = Answering (ActivePhase AnsweringData)
    | Reviewing (ActivePhase ReviewingData)
    | Finished FinishedState
    deriving (Show)

overActiveCore :: (ExamCore -> ExamCore) -> ExamPhase -> ExamPhase
overActiveCore f (Answering ap) = Answering (ap & activeCore %~ f)
overActiveCore f (Reviewing ap) = Reviewing (ap & activeCore %~ f)
overActiveCore _ p = p

finishExam :: ExamCore -> FinishedState
finishExam c =
    FinishedState
        { _finalScore = c ^. score
        , _finalTotal = totalQuestions c
        , _finalElapsed = c ^. elapsedSeconds
        }

totalQuestions :: ExamCore -> Int
totalQuestions c = V.length (c ^. questions)

initialState :: NonEmpty Question -> ExamPhase
initialState (q :| qs) =
    Answering
        ActivePhase
            { _activeCore = core
            , _activeQuestion = q
            , _phaseData =
                AnsweringData
                    { _selectedAnswers = IS.empty
                    , _focusedAnswer = 0
                    }
            }
  where
    core =
        ExamCore
            { _questions = V.fromList (q : qs)
            , _currentIndex = 0
            , _score = 0
            , _elapsedSeconds = 0
            }
