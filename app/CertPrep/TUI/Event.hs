module CertPrep.TUI.Event (
  CustomEvent (..),
  handleEvent,
  toggleAnswerPure,
  moveFocusPure,
  totalAnimFrames,
) where

import Brick
import Brick.Keybindings (
  KeyDispatcher,
  KeyEventHandler,
  handleKey,
  keyDispatcher,
  onEvent,
 )
import Data.IntSet qualified as IS
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (+~), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (+=), (.=))
import Prelude hiding (Down)

import CertPrep.Exam
import CertPrep.Explanations (MonadExplain (..))
import CertPrep.TUI.Keybindings (
  KeyEvent (..),
  defaultKeyConfig,
 )
import CertPrep.TUI.Monad (
  CustomEvent (..),
  TuiEnv (..),
  TuiM,
  liftEvent,
  tuiHalt,
  whenAnswering,
  whenCheckingTrophies,
  whenExplaining,
  whenReviewing,
  whenTrophyAwarded,
 )
import CertPrep.Types (Question (..), isCorrect)

totalAnimFrames :: Int
totalAnimFrames = 5

-- Each phase gets its own dispatcher so the same key can mean different things
-- per phase (e.g. Enter submits while answering, advances while reviewing).
-- Brick only checks for binding collisions within a single dispatcher, so the
-- shared KeyConfig may map Enter to several events without conflict.
mkDispatcher :: [KeyEventHandler KeyEvent TuiM] -> KeyDispatcher KeyEvent TuiM
mkDispatcher hs = case keyDispatcher defaultKeyConfig (globalHandlers ++ hs) of
  Right d -> d
  -- FIXME: This probably should be handled better.
  Left _ -> error "Invalid keybindings."

dispatcherFor :: ExamPhase -> KeyDispatcher KeyEvent TuiM
dispatcherFor phase = case phase of
  Answering _ -> answeringDispatcher
  Reviewing _ -> reviewingDispatcher
  Explaining _ -> explainingDispatcher
  CheckingTrophies _ -> checkingTrophiesDispatcher
  TrophyAwarded _ -> trophyDispatcher
  Finished _ -> finishedDispatcher

-- Available in every phase.
globalHandlers :: [KeyEventHandler KeyEvent TuiM]
globalHandlers = [onEvent QuitEvent "Quit the application" tuiHalt]

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

reviewingDispatcher :: KeyDispatcher KeyEvent TuiM
reviewingDispatcher =
  mkDispatcher
    [ onEvent
        ReviewNextQuestionEvent
        "Select Next Question"
        (modifyReviewing (travelToQuestion 1)),
      onEvent
        ReviewPreviousQuestionEvent
        "Select Previous Question"
        (modifyReviewing (travelToQuestion (-1))),
      onEvent FinishReviewEvent "Finish Review" (whenReviewing handleNextQuestion),
      onEvent
        RequestAiExplanationEvent
        "Request AI Explanation"
        ( whenReviewing $ \ap -> do
            liftEvent $ vScrollToBeginning explainScroll
            requestExplanationFor ap
        )
    ]

explainingDispatcher :: KeyDispatcher KeyEvent TuiM
explainingDispatcher =
  mkDispatcher
    [ onEvent NextAnswerEvent "Scroll Down" (liftEvent $ vScrollBy explainScroll 1),
      onEvent PreviousAnswerEvent "Scroll Up" (liftEvent $ vScrollBy explainScroll (-1)),
      onEvent ScrollUpEvent "Scroll Up" (liftEvent $ vScrollPage explainScroll Up),
      onEvent ScrollDownEvent "Scroll Down" (liftEvent $ vScrollPage explainScroll Down),
      onEvent DismissExplanationEvent "Back to Review" (whenExplaining handleExplaining)
    ]

checkingTrophiesDispatcher :: KeyDispatcher KeyEvent TuiM
checkingTrophiesDispatcher =
  mkDispatcher
    [onEvent ContinueEvent "Continue" (whenCheckingTrophies handleCheckTrophies)]

trophyDispatcher :: KeyDispatcher KeyEvent TuiM
trophyDispatcher =
  mkDispatcher
    [onEvent AcceptTrophyEvent "Dismiss Trophy" (whenTrophyAwarded handleTrophyDismiss)]

finishedDispatcher :: KeyDispatcher KeyEvent TuiM
finishedDispatcher = mkDispatcher [onEvent ContinueEvent "Quit" tuiHalt]

handleEvent :: BrickEvent Name CustomEvent -> TuiM ()
handleEvent (VtyEvent (V.EvKey key modifier)) = do
  phase <- use examPhase
  void $ handleKey (dispatcherFor phase) key modifier
handleEvent (MouseDown (AnswerChoice idx) _ _ _) =
  modifyAnswering $ \ap ->
    ap & phaseData . selectedAnswers %~ toggleAnswerPure idx
handleEvent (MouseDown SubmitButton _ _ _) = do
  phase <- use examPhase
  case phase of
    Answering ap -> handleSubmit ap
    _ -> pass
handleEvent (MouseDown NextButton _ _ _) = do
  phase <- use examPhase
  case phase of
    Reviewing ap -> handleNextQuestion ap
    _ -> pass
handleEvent (AppEvent Tick) = do
  phase <- use examPhase
  case phase of
    TrophyAwarded tad -> handleTrophyTick tad
    CheckingTrophies core -> handleCheckTrophies core
    _ -> examPhase %= overActiveCore (elapsedSeconds +~ 1)
handleEvent (AppEvent (ExplanationEvent rid ev)) = examPhase %= applyExplainEvent rid ev
handleEvent _ = pass

-- Brick drops scroll requests for viewports that aren't rendered, so these
-- are safe to issue in any phase; they only take effect while explaining.
explainScroll :: ViewportScroll Name
explainScroll = viewportScroll ExplanationViewport

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

handleNextQuestion :: ActivePhase ReviewingData -> TuiM ()
handleNextQuestion ap = handleCheckTrophies (ap ^. activeCore)

handleExplaining :: ActivePhase ExplainingData -> TuiM ()
handleExplaining ap = examPhase .= backToReview ap

requestExplanationFor ::
  (MonadState AppState m, MonadExplain m) => ActivePhase ReviewingData -> m ()
requestExplanationFor ap = do
  enabled <- explainAvailable
  when enabled $ do
    rid <- use nextExplainId
    nextExplainId += 1
    let (req, phase) = beginExplanation rid ap
    examPhase .= phase -- Pending shown immediately, before any network I/O
    requestExplanation req

handleCheckTrophies :: ExamCore -> TuiM ()
handleCheckTrophies core = do
  ts <- use trophyState
  earned <- use earnedTrophies
  let allTrophies = checkAllTrophies ts earned core
      nextPhase = advanceExam core

  case allTrophies of
    [] -> pass
    _ -> do
      cp <- asks tuiConfigPath
      newEarned <- persistTrophies allTrophies cp earned
      earnedTrophies .= newEarned

  examPhase .= wrapWithTrophies allTrophies nextPhase

handleTrophyDismiss :: TrophyAwardedData -> TuiM ()
handleTrophyDismiss tad = case tad ^. pendingTrophies of
  (t : ts) ->
    examPhase
      .= TrophyAwarded
        TrophyAwardedData {
          _awardedTrophy = t,
          _animationFrame = 0,
          _pendingTrophies = ts,
          _returnPhase = tad ^. returnPhase
        }
  [] -> do
    let rp = tad ^. returnPhase
    examPhase .= rp
    case rp of
      Answering newAp -> do
        let core = newAp ^. activeCore
            updatedCore =
              core & questionStartTime .~ core ^. elapsedSeconds
        examPhase .= Answering (newAp & activeCore .~ updatedCore)
      _ -> pass

handleTrophyTick :: TrophyAwardedData -> TuiM ()
handleTrophyTick tad = do
  let newFrame = tad ^. animationFrame + 1
  if newFrame >= totalAnimFrames then
    handleTrophyDismiss tad
  else
    examPhase .= TrophyAwarded (tad & animationFrame .~ newFrame)

modifyAnswering :: (ActivePhase AnsweringData -> ActivePhase AnsweringData) -> TuiM ()
modifyAnswering f = whenAnswering (\ap -> examPhase .= Answering (f ap))

modifyReviewing :: (ActivePhase ReviewingData -> ActivePhase ReviewingData) -> TuiM ()
modifyReviewing f = whenReviewing (\ap -> examPhase .= Reviewing (f ap))

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
