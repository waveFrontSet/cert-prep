module TUI.Event (
    CustomEvent (..),
    handleEvent,
    submitAnswer,
    nextQuestion,
    toggleAnswerPure,
    moveFocusPure,
    totalAnimFrames,
) where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (+~), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import State
import Trophy (
    FinalStatistics (..),
    TrophyDef (..),
    TrophyState (..),
    checkAfterSubmit,
    checkAtFinish,
    currentStreak,
    lastQuestionSeconds,
    saveEarnedTrophies,
    trophyDefId,
 )
import Types (Question (..), evalAnswer, isCorrect)

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
            TrophyAwarded tad -> handleTrophyDismiss tad
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
        _ -> examPhase %= overActiveCore (elapsedSeconds +~ 1)
handleEvent _ = return ()

handleSubmit :: ActivePhase AnsweringData -> EventM Name AppState ()
handleSubmit ap = do
    let q = ap ^. activeQuestion
        userAnswer = ap ^. phaseData . selectedAnswers
        core = ap ^. activeCore
        wasCorrect = isCorrect q userAnswer
        newCore =
            if wasCorrect
                then core & score +~ 1
                else core
        questionTime = (core ^. elapsedSeconds) - (core ^. questionStartTime)

    oldTrophyState@TrophyState{currentStreak = cs} <- use trophyState
    let newTrophyState =
            oldTrophyState
                { currentStreak = if wasCorrect then cs + 1 else 0
                , lastQuestionSeconds = questionTime
                }
    trophyState .= newTrophyState

    earned <- use earnedTrophies
    let newTrophies =
            filter
                (not . (`Set.member` earned) . trophyDefId)
                $ checkAfterSubmit
                    wasCorrect
                    newTrophyState

    let reviewPhase =
            Reviewing $
                ap
                    & activeCore .~ newCore
                    & phaseData
                        .~ ReviewingData
                            { _answerResult = evalAnswer q userAnswer
                            , _lastSelected = userAnswer
                            }

    case newTrophies of
        [] -> examPhase .= reviewPhase
        (t : ts) -> do
            let trophyIds = Set.fromList (map trophyDefId newTrophies)
            earnedTrophies %= Set.union trophyIds
            newEarned <- use earnedTrophies
            cp <- use configPath
            liftIO $ saveEarnedTrophies cp newEarned
            examPhase
                .= TrophyAwarded
                    TrophyAwardedData
                        { _awardedTrophy = t
                        , _animationFrame = 0
                        , _pendingTrophies = ts
                        , _returnPhase = reviewPhase
                        }

handleNextQuestion :: ActivePhase ReviewingData -> EventM Name AppState ()
handleNextQuestion ap = do
    let nextPhase = nextQuestion ap
    case nextPhase of
        Finished fs -> do
            earned <- use earnedTrophies
            let finishTrophies =
                    filter (not . (`Set.member` earned) . trophyDefId) $
                        checkAtFinish $
                            FinalStatistics (fs ^. finalScore) (fs ^. finalTotal)
            case finishTrophies of
                [] -> examPhase .= Finished fs
                (t : ts) -> do
                    let trophyIds =
                            Set.fromList (map trophyDefId finishTrophies)
                    earnedTrophies %= Set.union trophyIds
                    newEarned <- use earnedTrophies
                    cp <- use configPath
                    liftIO $ saveEarnedTrophies cp newEarned
                    examPhase
                        .= TrophyAwarded
                            TrophyAwardedData
                                { _awardedTrophy = t
                                , _animationFrame = 0
                                , _pendingTrophies = ts
                                , _returnPhase = Finished fs
                                }
        Answering newAp -> do
            let core = newAp ^. activeCore
                updatedCore = core & questionStartTime .~ (core ^. elapsedSeconds)
            examPhase .= Answering (newAp & activeCore .~ updatedCore)
        _ -> examPhase .= nextPhase

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
        -- If returning to Answering, reset questionStartTime
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
