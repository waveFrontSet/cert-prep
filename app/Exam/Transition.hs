module Exam.Transition (
    overActiveCore,
    finishExam,
    initialState,
    submitAnswer,
    nextQuestion,
    advanceExam,
)
where

import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector qualified as V
import Lens.Micro ((%~), (&), (+~), (.~), (^.))

import Exam.Core
import Trophy (EarnedTrophies, TrophyState (..))
import Types (Question (..), evalAnswer, isCorrect)

overActiveCore :: (ExamCore -> ExamCore) -> ExamPhase -> ExamPhase
overActiveCore f (Answering ap) = Answering (ap & activeCore %~ f)
overActiveCore f (Reviewing ap) = Reviewing (ap & activeCore %~ f)
overActiveCore f (CheckingTrophies core) = CheckingTrophies (f core)
overActiveCore _ p = p

finishExam :: ExamCore -> FinishedState
finishExam c =
    FinishedState
        { _finalScore = c ^. score
        , _finalTotal = totalQuestions c
        , _finalElapsed = c ^. elapsedSeconds
        }

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

submitAnswer :: ActivePhase AnsweringData -> ExamPhase
submitAnswer ap =
    let q = ap ^. activeQuestion
        userAnswer = ap ^. phaseData . selectedAnswers
        core = ap ^. activeCore
        newCore =
            if isCorrect q userAnswer
                then core & score +~ 1
                else core
     in Reviewing
            ActivePhase
                { _activeCore = newCore
                , _activeQuestion = q
                , _phaseData =
                    ReviewingData
                        { _answerResult = evalAnswer q userAnswer
                        , _lastSelected = userAnswer
                        }
                }

nextQuestion :: ActivePhase ReviewingData -> ExamPhase
nextQuestion ap = CheckingTrophies (ap ^. activeCore)

advanceExam :: ExamCore -> ExamPhase
advanceExam core =
    let nextIdx = core ^. currentIndex + 1
     in if nextIdx >= totalQuestions core
            then Finished (finishExam core)
            else
                let nextQ = (core ^. questions) V.! nextIdx
                    newCore =
                        core
                            & currentIndex .~ nextIdx
                            & questionStartTime .~ (core ^. elapsedSeconds)
                 in Answering
                        ActivePhase
                            { _activeCore = newCore
                            , _activeQuestion = nextQ
                            , _phaseData =
                                AnsweringData
                                    { _selectedAnswers = IS.empty
                                    , _focusedAnswer = 0
                                    }
                            }
