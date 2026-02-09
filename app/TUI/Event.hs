module TUI.Event (
    CustomEvent (..),
    handleEvent,
    toggleAnswerPure,
    moveFocusPure,
) where

import Brick
import Control.Monad (when)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (+=), (.=))
import State
import Types (Question (..), isCorrect)

data CustomEvent = Tick

handleEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey key [])) = case key of
    V.KEsc -> halt
    V.KChar 'q' -> halt
    V.KChar 'Q' -> halt
    V.KUp -> moveFocus (-1)
    V.KChar 'k' -> moveFocus (-1)
    V.KDown -> moveFocus 1
    V.KChar 'j' -> moveFocus 1
    V.KEnter -> do
        s <- get
        case s ^. phase of
            Answering -> submitAnswer
            Reviewing -> nextQuestion
            Finished -> halt
    V.KChar ' ' -> whenPhase Answering $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                s <- get
                let numAnswers = length (answerChoices q)
                    idx = s ^. focusedAnswer
                when (idx < numAnswers) $
                    selectedAnswers .= toggleAnswerPure idx (s ^. selectedAnswers)
            Nothing -> return ()
    _ -> return ()
handleEvent (MouseDown (AnswerChoice idx) _ _ _) =
    whenPhase Answering $ do
        sel <- use selectedAnswers
        selectedAnswers .= toggleAnswerPure idx sel
handleEvent (MouseDown SubmitButton _ _ _) =
    whenPhase Answering submitAnswer
handleEvent (MouseDown NextButton _ _ _) =
    whenPhase Reviewing nextQuestion
handleEvent (AppEvent Tick) = do
    p <- use phase
    when (p /= Finished) $ elapsedSeconds += 1
handleEvent _ = return ()

whenPhase :: Phase -> EventM Name AppState () -> EventM Name AppState ()
whenPhase p action = do
    current <- use phase
    when (current == p) action

toggleAnswerPure :: Int -> IntSet -> IntSet
toggleAnswerPure idx sel
    | IS.member idx sel = IS.delete idx sel
    | otherwise = IS.insert idx sel

moveFocusPure :: Int -> Int -> Int -> Int
moveFocusPure delta current numAnswers
    | numAnswers <= 0 = current
    | otherwise = (current + delta) `mod` numAnswers

moveFocus :: Int -> EventM Name AppState ()
moveFocus delta =
    whenPhase Answering $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                current <- use focusedAnswer
                let numAnswers = length (answerChoices q)
                focusedAnswer .= moveFocusPure delta current numAnswers
            Nothing -> return ()

submitAnswer :: EventM Name AppState ()
submitAnswer = do
    s <- get
    mQ <- gets currentQuestion
    case mQ of
        Just q -> do
            let userAnswer = s ^. selectedAnswers
            when (isCorrect q userAnswer) $ score += 1
            phase .= Reviewing
        Nothing -> return ()

nextQuestion :: EventM Name AppState ()
nextQuestion = do
    s <- get
    let nextIdx = s ^. currentIndex + 1
    if nextIdx >= totalQuestions s
        then phase .= Finished
        else do
            currentIndex .= nextIdx
            selectedAnswers .= IS.empty
            focusedAnswer .= 0
            phase .= Answering
