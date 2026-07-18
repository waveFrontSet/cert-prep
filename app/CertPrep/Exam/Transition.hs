module CertPrep.Exam.Transition (
    overActiveCore,
    finishExam,
    initialState,
    submitAnswer,
    nextQuestion,
    advanceExam,
    travelToQuestion,
    backToReview,
    beginExplanation,
    applyExplainEvent,
    stepExplanation,
)
where

import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Lens.Micro ((%~), (+~), (.~), (^.))

import CertPrep.Exam.Core
import CertPrep.Explanations (
    ExplainError (..),
    ExplainEvent (..),
    ExplainRequest (..),
    renderExplainError,
    renderExplainPrompt,
 )
import CertPrep.Trophy (EarnedTrophies, TrophyState (..))
import CertPrep.Types (Answer, Question (..), evalAnswer, isCorrect)

overActiveCore :: (ExamCore -> ExamCore) -> ExamPhase -> ExamPhase
overActiveCore f (Answering ap) = Answering (ap & activeCore %~ f)
overActiveCore f (Reviewing ap) = Reviewing (ap & activeCore %~ f)
overActiveCore f (Explaining ap) = Explaining (ap & activeCore %~ f)
overActiveCore f (CheckingTrophies core) = CheckingTrophies (f core)
overActiveCore _ p = p

finishExam :: ExamCore -> FinishedState
finishExam c =
    FinishedState
        { _finalScore = c ^. score
        , _finalTotal = totalQuestions c
        , _finalElapsed = c ^. elapsedSeconds
        }

initialState :: NonEmpty Question -> EarnedTrophies -> AppState
initialState (q :| qs) earned =
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
        , _nextExplainId = 0
        }
  where
    core =
        ExamCore
            { _questions = V.fromList (q : qs)
            , _currentIndex = 0
            , _score = 0
            , _elapsedSeconds = 0
            , _questionStartTime = 0
            , _userAnswers = V.empty
            }

submitAnswer :: ActivePhase AnsweringData -> ExamPhase
submitAnswer ap =
    let q = ap ^. activeQuestion
        userAnswer :: Answer
        userAnswer = ap ^. phaseData . selectedAnswers
        core = ap ^. activeCore
        newCore =
            core
                & userAnswers
                %~ (`V.snoc` userAnswer)
                    & score
                +~ (if isCorrect q userAnswer then 1 else 0)
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

travelToQuestion :: Int -> ActivePhase ReviewingData -> ActivePhase ReviewingData
travelToQuestion i ap =
    let core = ap ^. activeCore
        currIndex = core ^. currentIndex
        userAnswer = (core ^. userAnswers) V.! newIndex
        newIndex = min (V.length (core ^. userAnswers) - 1) (max 0 (currIndex + i))
        newCore = core & currentIndex .~ newIndex
        qs = core ^. questions
        q = qs V.! newIndex
        newPhaseData =
            ap
                ^. phaseData
                    & (answerResult .~ evalAnswer q userAnswer)
                    & (lastSelected .~ userAnswer)
     in ap
            & activeCore
            .~ newCore
                & activeQuestion
            .~ q
                & phaseData
            .~ newPhaseData

beginExplanation :: Int -> ActivePhase ReviewingData -> (ExplainRequest, ExamPhase)
beginExplanation rid ap = (ExplainRequest{reqId = rid, reqPrompt = prompt}, Explaining ap')
  where
    prompt = renderExplainPrompt (ap ^. activeQuestion) (ap ^. phaseData . answerResult)
    ap' =
        ap
            & phaseData
            .~ ExplainingData
                { _explainId = rid
                , _explanationStatus = ExplanationPending
                , _reviewingData = ap ^. phaseData
                }

applyExplainEvent :: Int -> ExplainEvent -> ExamPhase -> ExamPhase
applyExplainEvent rid ev (Explaining ap)
    | ap ^. phaseData . explainId == rid =
        Explaining $ ap & phaseData . explanationStatus %~ stepExplanation ev
applyExplainEvent _ _ p = p

stepExplanation :: ExplainEvent -> ExplanationStatus -> ExplanationStatus
stepExplanation (ExplainChunk t) ExplanationPending = ExplanationStreaming t
stepExplanation (ExplainChunk t) (ExplanationStreaming acc) = ExplanationStreaming (acc <> t)
stepExplanation ExplainDone (ExplanationStreaming acc) = ExplanationSuccess acc
stepExplanation ExplainDone ExplanationPending = ExplanationFailure (renderExplainError ExplainEmptyResponse)
stepExplanation (ExplainFailed e) ExplanationPending = ExplanationFailure (renderExplainError e)
stepExplanation (ExplainFailed e) (ExplanationStreaming _) = ExplanationFailure (renderExplainError e)
-- Success and Failure are terminal; late events from the stream are ignored.
stepExplanation _ status = status

backToReview :: ActivePhase ExplainingData -> ExamPhase
backToReview ap = Reviewing (ap & phaseData .~ reviewData)
  where
    reviewData = ap ^. phaseData . reviewingData

nextQuestion :: ActivePhase ReviewingData -> ExamPhase
nextQuestion ap = CheckingTrophies (ap ^. activeCore)

advanceExam :: ExamCore -> ExamPhase
advanceExam core =
    let nextIdx = length $ core ^. userAnswers
     in if nextIdx >= totalQuestions core
            then Finished (finishExam core)
            else
                let nextQ = (core ^. questions) V.! nextIdx
                    newCore =
                        core
                            & currentIndex
                            .~ nextIdx
                                & questionStartTime
                            .~ (core ^. elapsedSeconds)
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
