module TUI.Event (
    CustomEvent (..),
    handleEvent,
    toggleAnswerPure,
    moveFocusPure,
    totalAnimFrames,
) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (+~), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))

import Exam.Core
import Exam.Transition (advanceExam, overActiveCore, submitAnswer, travelToQuestion)
import Exam.Trophy (
    checkAllTrophies,
    persistTrophies,
    updateTrophyState,
    wrapWithTrophies,
 )
import Types (Question (..), isCorrect)

data CustomEvent = Tick

totalAnimFrames :: Int
totalAnimFrames = 5

handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey key [])) = case key of
    V.KEsc -> halt
    V.KChar 'q' -> halt
    V.KChar 'Q' -> halt
    V.KEnter -> do
        phase <- use examPhase
        case phase of
            Answering ap -> handleSubmit ap
            Reviewing ap -> handleNextQuestion ap
            CheckingTrophies core -> handleCheckTrophies core
            TrophyAwarded tad -> handleTrophyDismiss tad
            Finished _ -> halt
    V.KUp -> modifyAnswering (moveFocus (-1))
    V.KChar 'k' -> modifyAnswering (moveFocus (-1))
    V.KDown -> modifyAnswering (moveFocus 1)
    V.KChar 'j' -> modifyAnswering (moveFocus 1)
    V.KChar ' ' -> modifyAnswering toggleSelected
    V.KChar 'l' -> modifyReviewing (travelToQuestion 1)
    V.KChar 'h' -> modifyReviewing (travelToQuestion (-1))
    _ -> return ()
handleEvent (MouseDown (AnswerChoice idx) _ _ _) =
    modifyAnswering $ \ap ->
        ap & phaseData . selectedAnswers %~ toggleAnswerPure idx
handleEvent (MouseDown SubmitButton _ _ _) = do
    phase <- use examPhase
    case phase of
        Answering ap -> handleSubmit ap
        _ -> return ()
handleEvent (MouseDown NextButton _ _ _) = do
    phase <- use examPhase
    case phase of
        Reviewing ap -> handleNextQuestion ap
        _ -> return ()
handleEvent (AppEvent Tick) = do
    phase <- use examPhase
    case phase of
        TrophyAwarded tad -> handleTrophyTick tad
        CheckingTrophies core -> handleCheckTrophies core
        _ -> examPhase %= overActiveCore (elapsedSeconds +~ 1)
handleEvent _ = return ()

handleSubmit :: ActivePhase AnsweringData -> EventM Name AppState ()
handleSubmit ap = do
    let q = ap ^. activeQuestion
        userAnswer = ap ^. phaseData . selectedAnswers
        wasCorrect = isCorrect q userAnswer
        core = ap ^. activeCore
        questionTime = (core ^. elapsedSeconds) - (core ^. questionStartTime)

    oldTS <- use trophyState
    trophyState .= updateTrophyState wasCorrect questionTime oldTS

    examPhase .= submitAnswer ap

handleNextQuestion :: ActivePhase ReviewingData -> EventM Name AppState ()
handleNextQuestion ap = handleCheckTrophies (ap ^. activeCore)

handleCheckTrophies :: ExamCore -> EventM Name AppState ()
handleCheckTrophies core = do
    ts <- use trophyState
    earned <- use earnedTrophies
    let allTrophies = checkAllTrophies ts earned core
        nextPhase = advanceExam core

    case allTrophies of
        [] -> pure ()
        _ -> do
            cp <- use configPath
            newEarned <- liftIO $ persistTrophies allTrophies cp earned
            earnedTrophies .= newEarned

    examPhase .= wrapWithTrophies allTrophies nextPhase

handleTrophyDismiss :: TrophyAwardedData -> EventM Name AppState ()
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
                        core & questionStartTime .~ (core ^. elapsedSeconds)
                examPhase .= Answering (newAp & activeCore .~ updatedCore)
            _ -> return ()

handleTrophyTick :: TrophyAwardedData -> EventM Name AppState ()
handleTrophyTick tad = do
    let newFrame = tad ^. animationFrame + 1
    if newFrame >= totalAnimFrames
        then handleTrophyDismiss tad
        else examPhase .= TrophyAwarded (tad & animationFrame .~ newFrame)

modifyAnswering ::
    (ActivePhase AnsweringData -> ActivePhase AnsweringData) ->
    EventM Name AppState ()
modifyAnswering f = do
    phase <- use examPhase
    case phase of
        Answering ap -> examPhase .= Answering (f ap)
        _ -> return ()

modifyReviewing ::
    (ActivePhase ReviewingData -> ActivePhase ReviewingData) ->
    EventM Name AppState ()
modifyReviewing f = do
    phase <- use examPhase
    case phase of
        Reviewing ap -> examPhase .= Reviewing (f ap)
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
