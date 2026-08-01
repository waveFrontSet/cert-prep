module CertPrep.TUI.Event.Finished (finishedDispatcher) where

import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )

import CertPrep.TUI.Event.Core (mkDispatcher)
import CertPrep.TUI.Event.Export (handleOpenExport)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiM,
  tuiHalt,
  whenFinished,
 )

finishedDispatcher :: KeyDispatcher KeyEvent TuiM
finishedDispatcher =
  mkDispatcher
    [ onEvent ContinueEvent "Quit" tuiHalt,
      onEvent OpenExportDialogEvent "Export Report" (whenFinished handleOpenExport)
    ]
