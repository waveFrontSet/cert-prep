{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module State (
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
import Trophy (EarnedTrophies, TrophyDef, TrophyState (..))
import Types (AnswerResult, Question)

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

initialState :: NonEmpty Question -> FilePath -> EarnedTrophies -> AppState
initialState (q :| qs) cfgPath earned =
    AppState
        { _examPhase =
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
        , _trophyState = TrophyState{currentStreak = 0, lastQuestionSeconds = 0}
        , _earnedTrophies = earned
        , _configPath = cfgPath
        }
  where
    core =
        ExamCore
            { _questions = V.fromList (q : qs)
            , _currentIndex = 0
            , _score = 0
            , _elapsedSeconds = 0
            , _questionStartTime = 0
            }
