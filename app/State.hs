{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module State (
    Name (..),
    ExamCore (..),
    AnsweringData (..),
    ReviewingData (..),
    TrophyData (..),
    ActivePhase (..),
    FinishedState (..),
    ExamPhase (..),
    questions,
    currentIndex,
    score,
    elapsedSeconds,
    questionStartedAt,
    correctStreak,
    unlockedTrophyIds,
    availableTrophies,
    selectedAnswers,
    focusedAnswer,
    activeCore,
    activeQuestion,
    phaseData,
    answerResult,
    lastSelected,
    newlyUnlocked,
    trophyToShow,
    trophyAnimationFrame,
    remainingTrophies,
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Lens.Micro ((%~), (&), (^.))
import Lens.Micro.TH (makeLenses)
import Types (AnswerResult, Question, Trophy)

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
    , _questionStartedAt :: Int
    , _correctStreak :: Int
    , _unlockedTrophyIds :: Set Text
    , _availableTrophies :: [Trophy]
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
    , _newlyUnlocked :: [Trophy]
    }
    deriving (Show)

makeLenses ''ReviewingData

data TrophyData = TrophyData
    { _trophyToShow :: Trophy
    , _trophyAnimationFrame :: Int
    , _remainingTrophies :: [Trophy]
    }
    deriving (Show)

makeLenses ''TrophyData

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
    | TrophyReveal (ActivePhase TrophyData)
    | Finished FinishedState
    deriving (Show)

overActiveCore :: (ExamCore -> ExamCore) -> ExamPhase -> ExamPhase
overActiveCore f (Answering ap) = Answering (ap & activeCore %~ f)
overActiveCore f (Reviewing ap) = Reviewing (ap & activeCore %~ f)
overActiveCore f (TrophyReveal ap) = TrophyReveal (ap & activeCore %~ f)
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

initialState :: NonEmpty Question -> [Trophy] -> ExamPhase
initialState (q :| qs) trophies =
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
            , _questionStartedAt = 0
            , _correctStreak = 0
            , _unlockedTrophyIds = Set.empty
            , _availableTrophies = trophies
            }
