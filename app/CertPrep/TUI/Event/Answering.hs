module CertPrep.TUI.Event.Answering (
  answeringDispatcher,
  handleSubmit,
  handleAnswerClick,
  toggleAnswerPure,
  moveFocusPure,
) where

import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Data.IntSet qualified as IS
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Mtl (use, (.=))

import CertPrep.Exam
import CertPrep.TUI.Event.Core (mkDispatcher)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiM,
  whenAnswering,
 )
import CertPrep.Types (Question (..), isCorrect)

answeringDispatcher :: KeyDispatcher KeyEvent TuiM
answeringDispatcher =
  mkDispatcher
    [ onEvent NextAnswerEvent "Select Next Answer" (modifyAnswering (moveFocus 1)),
      onEvent PreviousAnswerEvent "Select Previous Answer" (modifyAnswering (moveFocus (-1))),
      onEvent
        ToggleSelectedAnswerEvent
        "Toggle Selected Answer"
        (modifyAnswering toggleSelected),
      onEvent SubmitAnswersEvent "Submit Answers" (whenAnswering handleSubmit)
    ]

handleSubmit :: ActivePhase AnsweringData -> TuiM ()
handleSubmit ap = do
  let q = ap ^. activeQuestion
      userAnswer = ap ^. phaseData . selectedAnswers
      wasCorrect = isCorrect q userAnswer
      core = ap ^. activeCore
      questionTime = core ^. elapsedSeconds - core ^. questionStartTime

  oldTS <- use trophyState
  trophyState .= updateTrophyState wasCorrect questionTime oldTS

  examPhase .= submitAnswer ap

-- A click toggles the answer it lands on, whichever one currently has key focus.
handleAnswerClick :: Int -> TuiM ()
handleAnswerClick idx =
  modifyAnswering $ \ap ->
    ap & phaseData . selectedAnswers %~ toggleAnswerPure idx

modifyAnswering :: (ActivePhase AnsweringData -> ActivePhase AnsweringData) -> TuiM ()
modifyAnswering f = whenAnswering (\ap -> examPhase .= Answering (f ap))

toggleSelected :: ActivePhase AnsweringData -> ActivePhase AnsweringData
toggleSelected ap =
  let q = ap ^. activeQuestion
      numAnswers = length (answerChoices q)
      idx = ap ^. phaseData . focusedAnswer
   in if idx < numAnswers then
        ap & phaseData . selectedAnswers %~ toggleAnswerPure idx
      else
        ap

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
