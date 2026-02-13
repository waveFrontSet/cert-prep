module TUI.Event (
    CustomEvent (..),
    handleEvent,
    submitAnswer,
    nextQuestion,
    continueFromTrophy,
    toggleAnswerPure,
    moveFocusPure,
) where

import Brick
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Set qualified as Set
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (.~), (^.))
import State
import Types (Question (..), Trophy (..), TrophyCondition (..), evalAnswer, isCorrect)

data CustomEvent
    = SecondTick
    | AnimationTick

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
            TrophyReveal ap -> put (continueFromTrophy ap)
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
        TrophyReveal ap -> put (continueFromTrophy ap)
        _ -> return ()
handleEvent (AppEvent SecondTick) =
    modify $ overActiveCore (elapsedSeconds %~ (+ 1))
handleEvent (AppEvent AnimationTick) =
    modify updateAnimation
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
        answerIsCorrect = isCorrect q userAnswer
        responseSeconds = core ^. elapsedSeconds - core ^. questionStartedAt
        newCore =
            core
                & score %~ (\s -> if answerIsCorrect then s + 1 else s)
                & correctStreak .~ if answerIsCorrect then core ^. correctStreak + 1 else 0
        unlockedNow = unlockedTrophies newCore answerIsCorrect responseSeconds
        unlockedIdsNow = Set.fromList (map trophyId unlockedNow)
     in Reviewing
            ActivePhase
                { _activeCore = newCore & unlockedTrophyIds %~ Set.union unlockedIdsNow
                , _activeQuestion = q
                , _phaseData =
                    ReviewingData
                        { _answerResult = evalAnswer q userAnswer
                        , _lastSelected = userAnswer
                        , _newlyUnlocked = unlockedNow
                        }
                }

nextQuestion :: ActivePhase ReviewingData -> ExamPhase
nextQuestion ap =
    case ap ^. phaseData . newlyUnlocked of
        t : ts ->
            TrophyReveal
                ActivePhase
                    { _activeCore = ap ^. activeCore
                    , _activeQuestion = ap ^. activeQuestion
                    , _phaseData =
                        TrophyData
                            { _trophyToShow = t
                            , _trophyAnimationFrame = 0
                            , _remainingTrophies = ts
                            }
                    }
        [] -> advanceToNextQuestion (ap ^. activeCore)

continueFromTrophy :: ActivePhase TrophyData -> ExamPhase
continueFromTrophy ap =
    case ap ^. phaseData . remainingTrophies of
        t : ts ->
            TrophyReveal
                ActivePhase
                    { _activeCore = ap ^. activeCore
                    , _activeQuestion = ap ^. activeQuestion
                    , _phaseData =
                        TrophyData
                            { _trophyToShow = t
                            , _trophyAnimationFrame = 0
                            , _remainingTrophies = ts
                            }
                    }
        [] -> advanceToNextQuestion (ap ^. activeCore)

advanceToNextQuestion :: ExamCore -> ExamPhase
advanceToNextQuestion core =
    let nextIdx = core ^. currentIndex + 1
     in if nextIdx >= totalQuestions core
            then Finished (finishExam core)
            else
                let nextQ = (core ^. questions) Vec.! nextIdx
                 in Answering
                        ActivePhase
                            { _activeCore =
                                core
                                    & currentIndex .~ nextIdx
                                    & questionStartedAt .~ (core ^. elapsedSeconds)
                            , _activeQuestion = nextQ
                            , _phaseData =
                                AnsweringData
                                    { _selectedAnswers = IS.empty
                                    , _focusedAnswer = 0
                                    }
                            }

updateAnimation :: ExamPhase -> ExamPhase
updateAnimation (TrophyReveal ap) =
    TrophyReveal (ap & phaseData . trophyAnimationFrame %~ (\f -> (f + 1) `mod` 8))
updateAnimation p = p

unlockedTrophies :: ExamCore -> Bool -> Int -> [Trophy]
unlockedTrophies core answerIsCorrect responseSeconds =
    filter (\t -> Set.notMember (trophyId t) (core ^. unlockedTrophyIds))
        $ filter (isUnlockedBy core answerIsCorrect responseSeconds) (core ^. availableTrophies)

isUnlockedBy :: ExamCore -> Bool -> Int -> Trophy -> Bool
isUnlockedBy core answerIsCorrect responseSeconds trophy =
    case trophyCondition trophy of
        CorrectStreakAtLeast n -> core ^. correctStreak >= n
        TotalCorrectAtLeast n -> core ^. score >= n
        FastCorrectAtMost n -> answerIsCorrect && responseSeconds <= n
