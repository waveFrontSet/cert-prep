module CertPrep.TUI.Event (
    CustomEvent (..),
    handleEvent,
    toggleAnswerPure,
    moveFocusPure,
    totalAnimFrames,
) where

import Brick
import Data.IntSet qualified as IS
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (+~), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (+=), (.=))
import Prelude hiding (Down)

import CertPrep.Exam.Core
import CertPrep.Exam.Transition (
    advanceExam,
    applyExplainEvent,
    backToReview,
    beginExplanation,
    overActiveCore,
    submitAnswer,
    travelToQuestion,
 )
import CertPrep.Exam.Trophy (
    checkAllTrophies,
    persistTrophies,
    updateTrophyState,
    wrapWithTrophies,
 )
import CertPrep.Explanations (MonadExplain (..))
import CertPrep.TUI.Monad (
    CustomEvent (..),
    TuiEnv (..),
    TuiM,
    liftEvent,
    tuiHalt,
    whenAnswering,
    whenReviewing,
 )
import CertPrep.Types (Question (..), isCorrect)

totalAnimFrames :: Int
totalAnimFrames = 5

handleEvent :: BrickEvent Name CustomEvent -> TuiM ()
handleEvent (VtyEvent (V.EvKey key [])) = case key of
    V.KEsc -> tuiHalt
    V.KChar 'q' -> tuiHalt
    V.KChar 'Q' -> tuiHalt
    V.KEnter -> do
        phase <- use examPhase
        case phase of
            Answering ap -> handleSubmit ap
            Reviewing ap -> handleNextQuestion ap
            Explaining ap -> handleExplaining ap
            CheckingTrophies core -> handleCheckTrophies core
            TrophyAwarded tad -> handleTrophyDismiss tad
            Finished _ -> tuiHalt
    V.KUp -> moveFocusOrScroll (-1)
    V.KChar 'k' -> moveFocusOrScroll (-1)
    V.KDown -> moveFocusOrScroll 1
    V.KChar 'j' -> moveFocusOrScroll 1
    V.KChar ' ' -> modifyAnswering toggleSelected
    V.KChar 'l' -> modifyReviewing (travelToQuestion 1)
    V.KChar 'h' -> modifyReviewing (travelToQuestion (-1))
    V.KChar 'a' -> whenReviewing $ \ap -> do
        liftEvent $ vScrollToBeginning explainScroll -- don't inherit the previous explanation's offset
        requestExplanationFor ap
    _ -> pass
handleEvent (VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) = liftEvent $ vScrollPage explainScroll Down
handleEvent (VtyEvent (V.EvKey (V.KChar 'b') [V.MCtrl])) = liftEvent $ vScrollPage explainScroll Up
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

-- Up/Down move the answer focus while answering and scroll the explanation
-- while explaining.
moveFocusOrScroll :: Int -> TuiM ()
moveFocusOrScroll delta = do
    phase <- use examPhase
    case phase of
        Answering ap -> examPhase .= Answering (moveFocus delta ap)
        Explaining _ -> liftEvent $ vScrollBy explainScroll delta
        _ -> pass

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
                TrophyAwardedData
                    { _awardedTrophy = t
                    , _animationFrame = 0
                    , _pendingTrophies = ts
                    , _returnPhase = tad ^. returnPhase
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
    if newFrame >= totalAnimFrames
        then handleTrophyDismiss tad
        else examPhase .= TrophyAwarded (tad & animationFrame .~ newFrame)

modifyAnswering :: (ActivePhase AnsweringData -> ActivePhase AnsweringData) -> TuiM ()
modifyAnswering f = whenAnswering (\ap -> examPhase .= Answering (f ap))

modifyReviewing :: (ActivePhase ReviewingData -> ActivePhase ReviewingData) -> TuiM ()
modifyReviewing f = whenReviewing (\ap -> examPhase .= Reviewing (f ap))

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
