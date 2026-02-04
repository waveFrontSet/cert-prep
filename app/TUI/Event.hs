module TUI.Event (handleEvent) where

import Brick
import Control.Monad (when)
import Data.IntSet qualified as IS
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (+=), (.=))
import TUI.State
import Types (Question (..), isCorrect)

handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    s <- get
    case s ^. phase of
        Answering -> submitAnswer
        Reviewing -> nextQuestion
        Finished -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    s <- get
    when (s ^. phase == Answering) $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                let numAnswers = length (questionAnswerChoices q)
                    idx = s ^. focusedAnswer
                when (idx < numAnswers) $ toggleAnswer idx
            Nothing -> return ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) = moveFocus (-1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = moveFocus 1
handleEvent (MouseDown (AnswerChoice idx) _ _ _) = do
    s <- get
    when (s ^. phase == Answering) $ toggleAnswer idx
handleEvent (MouseDown SubmitButton _ _ _) = do
    s <- get
    when (s ^. phase == Answering) submitAnswer
handleEvent (MouseDown NextButton _ _ _) = do
    s <- get
    when (s ^. phase == Reviewing) nextQuestion
handleEvent _ = return ()

toggleAnswer :: Int -> EventM Name AppState ()
toggleAnswer idx = do
    sel <- use selectedAnswers
    if IS.member idx sel
        then selectedAnswers .= IS.delete idx sel
        else selectedAnswers .= IS.insert idx sel

moveFocus :: Int -> EventM Name AppState ()
moveFocus delta = do
    s <- get
    when (s ^. phase == Answering) $ do
        mQ <- gets currentQuestion
        case mQ of
            Just q -> do
                let numAnswers = length (questionAnswerChoices q)
                    current = s ^. focusedAnswer
                    new = (current + delta) `mod` numAnswers
                focusedAnswer .= new
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
