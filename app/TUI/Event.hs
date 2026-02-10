module TUI.Event (
    CustomEvent (..),
    handleEvent,
    submitAnswer,
    nextQuestion,
    toggleAnswerPure,
    moveFocusPure,
) where

import Brick
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (.~), (^.))
import State
import Types (Question (..), evalAnswer, isCorrect)

data CustomEvent = Tick

handleEvent :: BrickEvent Name CustomEvent -> EventM Name ExamPhase ()
handleEvent (VtyEvent (V.EvKey key [])) = case key of
    V.KEsc -> halt
    V.KChar 'q' -> halt
    V.KChar 'Q' -> halt
    V.KEnter -> do
        s <- get
        case s of
            Answering ap -> put (submitAnswer ap)
            Reviewing ap -> put (nextQuestion ap)
            Finished _ -> halt
    V.KUp -> modifyAnswering (moveFocus (-1))
    V.KChar 'k' -> modifyAnswering (moveFocus (-1))
    V.KDown -> modifyAnswering (moveFocus 1)
    V.KChar 'j' -> modifyAnswering (moveFocus 1)
    V.KChar ' ' -> modifyAnswering toggleSelected
    _ -> return ()
handleEvent (MouseDown (AnswerChoice idx) _ _ _) =
    modifyAnswering $ \ap ->
        ap & phaseData . selectedAnswers %~ toggleAnswerPure idx
handleEvent (MouseDown SubmitButton _ _ _) = do
    s <- get
    case s of
        Answering ap -> put (submitAnswer ap)
        _ -> return ()
handleEvent (MouseDown NextButton _ _ _) = do
    s <- get
    case s of
        Reviewing ap -> put (nextQuestion ap)
        _ -> return ()
handleEvent (AppEvent Tick) =
    modify $ overActiveCore (elapsedSeconds %~ (+ 1))
handleEvent _ = return ()

modifyAnswering ::
    (ActivePhase AnsweringData -> ActivePhase AnsweringData) ->
    EventM Name ExamPhase ()
modifyAnswering f = do
    s <- get
    case s of
        Answering ap -> put (Answering (f ap))
        _ -> return ()

toggleSelected :: ActivePhase AnsweringData -> ActivePhase AnsweringData
toggleSelected ap =
    let q = ap ^. activeQuestion
        numAnswers = length (answerChoices q)
        idx = ap ^. phaseData . focusedAnswer
     in if idx < numAnswers
            then ap & phaseData . selectedAnswers %~ toggleAnswerPure idx
            else ap

moveFocus :: Int -> ActivePhase AnsweringData -> ActivePhase AnsweringData
moveFocus delta ap =
    let q = ap ^. activeQuestion
        numAnswers = length (answerChoices q)
        current = ap ^. phaseData . focusedAnswer
     in ap & phaseData . focusedAnswer .~ moveFocusPure delta current numAnswers

toggleAnswerPure :: Int -> IntSet -> IntSet
toggleAnswerPure idx sel
    | IS.member idx sel = IS.delete idx sel
    | otherwise = IS.insert idx sel

moveFocusPure :: Int -> Int -> Int -> Int
moveFocusPure delta current numAnswers
    | numAnswers <= 0 = current
    | otherwise = (current + delta) `mod` numAnswers

submitAnswer :: ActivePhase AnsweringData -> ExamPhase
submitAnswer ap =
    let q = ap ^. activeQuestion
        userAnswer = ap ^. phaseData . selectedAnswers
        core = ap ^. activeCore
        newCore =
            if isCorrect q userAnswer
                then core & score %~ (+ 1)
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
nextQuestion ap =
    let core = ap ^. activeCore
        nextIdx = core ^. currentIndex + 1
     in if nextIdx >= totalQuestions core
            then Finished (finishExam core)
            else
                let nextQ = (core ^. questions) Vec.! nextIdx
                 in Answering
                        ActivePhase
                            { _activeCore = core & currentIndex .~ nextIdx
                            , _activeQuestion = nextQ
                            , _phaseData =
                                AnsweringData
                                    { _selectedAnswers = IS.empty
                                    , _focusedAnswer = 0
                                    }
                            }
