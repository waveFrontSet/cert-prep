{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Exam.Core (
    Name (..),
    ExamCore (..),
    AnsweringData (..),
    ReviewingData (..),
    ActivePhase (..),
    FinishedState (..),
    TrophyAwardedData (..),
    ExamPhase (..),
    AppState (..),
    questions,
    currentIndex,
    score,
    elapsedSeconds,
    questionStartTime,
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
    awardedTrophy,
    animationFrame,
    pendingTrophies,
    returnPhase,
    examPhase,
    trophyState,
    earnedTrophies,
    configPath,
    totalQuestions,
    userAnswers,
)
where

import Data.IntSet (IntSet)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Trophy (EarnedTrophies, TrophyDef, TrophyState (..))
import Types (Answer, AnswerResult, Question)

data Name
    = AnswerChoice Int
    | SubmitButton
    | NextButton
    | TrophyDismiss
    deriving (Show, Eq, Ord)

data ExamCore = ExamCore
    { _questions :: Vector Question
    , _currentIndex :: Int
    , _score :: Int
    , _elapsedSeconds :: Int
    , _questionStartTime :: Int
    , _userAnswers :: Vector Answer
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

-- TrophyAwardedData and ExamPhase are mutually recursive,
-- so they must be in the same TH splice group.

data TrophyAwardedData = TrophyAwardedData
    { _awardedTrophy :: TrophyDef
    , _animationFrame :: Int
    , _pendingTrophies :: [TrophyDef]
    , _returnPhase :: ExamPhase
    }
    deriving (Show)

data ExamPhase
    = Answering (ActivePhase AnsweringData)
    | Reviewing (ActivePhase ReviewingData)
    | CheckingTrophies ExamCore
    | TrophyAwarded TrophyAwardedData
    | Finished FinishedState
    deriving (Show)

makeLenses ''TrophyAwardedData

data AppState = AppState
    { _examPhase :: ExamPhase
    , _trophyState :: TrophyState
    , _earnedTrophies :: EarnedTrophies
    , _configPath :: FilePath
    }
    deriving (Show)

makeLenses ''AppState

totalQuestions :: ExamCore -> Int
totalQuestions c = V.length (c ^. questions)
