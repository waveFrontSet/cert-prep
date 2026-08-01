module CertPrep.TUI.Event.Trophy (
  checkingTrophiesDispatcher,
  trophyDispatcher,
  handleCheckTrophies,
  handleTrophyTick,
  totalAnimFrames,
) where

import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Lens.Micro ((.~), (^.))
import Lens.Micro.Mtl (use, (.=))

import CertPrep.Exam
import CertPrep.TUI.Event.Core (mkDispatcher)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiEnv (..),
  TuiM,
  whenCheckingTrophies,
  whenTrophyAwarded,
 )

totalAnimFrames :: Int
totalAnimFrames = 5

checkingTrophiesDispatcher :: KeyDispatcher KeyEvent TuiM
checkingTrophiesDispatcher =
  mkDispatcher
    [onEvent ContinueEvent "Continue" (whenCheckingTrophies handleCheckTrophies)]

trophyDispatcher :: KeyDispatcher KeyEvent TuiM
trophyDispatcher =
  mkDispatcher
    [onEvent AcceptTrophyEvent "Dismiss Trophy" (whenTrophyAwarded handleTrophyDismiss)]

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
