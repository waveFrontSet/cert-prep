module CertPrep.TUI.Event (
  CustomEvent (..),
  handleEvent,
) where

import Brick (BrickEvent (..))
import Brick.Keybindings (
  KeyDispatcher,
  handleKey,
 )
import Graphics.Vty qualified as V
import Lens.Micro ((+~))
import Lens.Micro.Mtl (use, (%=))

import CertPrep.Exam
import CertPrep.TUI.Event.Answering (
  answeringDispatcher,
  handleAnswerClick,
  handleSubmit,
 )
import CertPrep.TUI.Event.Explaining (explainingDispatcher)
import CertPrep.TUI.Event.Export (exportingDispatcher, handleExportInput)
import CertPrep.TUI.Event.Finished (finishedDispatcher)
import CertPrep.TUI.Event.Reviewing (handleNextQuestion, reviewingDispatcher)
import CertPrep.TUI.Event.Trophy (
  checkingTrophiesDispatcher,
  handleCheckTrophies,
  handleTrophyTick,
  trophyDispatcher,
 )
import CertPrep.TUI.Keybindings (KeyEvent)
import CertPrep.TUI.Monad (
  TuiM,
  whenExporting,
 )

dispatcherFor :: ExamPhase -> KeyDispatcher KeyEvent TuiM
dispatcherFor phase = case phase of
  Answering _ -> answeringDispatcher
  Reviewing _ -> reviewingDispatcher
  Explaining _ -> explainingDispatcher
  CheckingTrophies _ -> checkingTrophiesDispatcher
  TrophyAwarded _ -> trophyDispatcher
  Finished _ -> finishedDispatcher
  Exporting _ -> exportingDispatcher

handleEvent :: BrickEvent Name CustomEvent -> TuiM ()
handleEvent (VtyEvent ev@(V.EvKey key modifier)) = do
  phase <- use examPhase
  handled <- handleKey (dispatcherFor phase) key modifier
  -- Keys the dialog's dispatcher doesn't claim are editing input for
  -- the focused widget (brick editor / format list).
  unless handled $ whenExporting (handleExportInput ev)
handleEvent (MouseDown (AnswerChoice idx) _ _ _) = handleAnswerClick idx
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
