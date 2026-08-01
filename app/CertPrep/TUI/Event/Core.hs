module CertPrep.TUI.Event.Core (
  mkDispatcher,
  mkBareDispatcher,
  globalHandlers,
) where

import Brick.Keybindings (
  KeyDispatcher,
  KeyEventHandler,
  keyDispatcher,
  onEvent,
 )

import CertPrep.TUI.Keybindings (
  KeyEvent (..),
  defaultKeyConfig,
 )
import CertPrep.TUI.Monad (
  TuiM,
  tuiHalt,
 )

-- Each phase gets its own dispatcher so the same key can mean different things
-- per phase (e.g. Enter submits while answering, advances while reviewing).
-- Brick only checks for binding collisions within a single dispatcher, so the
-- shared KeyConfig may map Enter to several events without conflict.
mkDispatcher :: [KeyEventHandler KeyEvent TuiM] -> KeyDispatcher KeyEvent TuiM
mkDispatcher = mkBareDispatcher . (globalHandlers ++)

-- A dispatcher without the global handlers, for phases where plain
-- characters must reach a text input instead of e.g. 'q' → quit.
mkBareDispatcher :: [KeyEventHandler KeyEvent TuiM] -> KeyDispatcher KeyEvent TuiM
mkBareDispatcher hs = case keyDispatcher defaultKeyConfig hs of
  Right d -> d
  -- FIXME: This probably should be handled better.
  Left _ -> error "Invalid keybindings."

-- Available in every phase.
globalHandlers :: [KeyEventHandler KeyEvent TuiM]
globalHandlers = [onEvent QuitEvent "Quit the application" tuiHalt]
