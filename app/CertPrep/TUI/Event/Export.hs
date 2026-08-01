module CertPrep.TUI.Event.Export (
  exportingDispatcher,
  handleOpenExport,
  handleExportInput,
) where

import Brick (BrickEvent (VtyEvent), nestEventM')
import Brick.Focus qualified as F
import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Control.Exception (IOException, try)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (.~), (^.))
import Lens.Micro.Mtl ((.=))

import CertPrep.Exam
import CertPrep.Export (writeExport)
import CertPrep.TUI.Event.Core (mkBareDispatcher)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiM,
  liftEvent,
  whenExporting,
 )

-- The export dialog dispatches its own (configurable) bindings only;
-- global handlers are omitted so plain characters fall through to the
-- focused widget (e.g. 'q' must be typable in the filename editor).
exportingDispatcher :: KeyDispatcher KeyEvent TuiM
exportingDispatcher =
  mkBareDispatcher
    [ onEvent ExportConfirmEvent "Save Report" (whenExporting handleExportConfirm),
      onEvent
        ExportCancelEvent
        "Cancel Export"
        (whenExporting (\ed -> examPhase .= cancelExport ed)),
      onEvent
        ExportSwitchFieldEvent
        "Switch Field"
        ( whenExporting
            (\ed -> examPhase .= Exporting (ed & exportDialog . exportFocus %~ F.focusNext))
        )
    ]

handleOpenExport :: FinishedState -> TuiM ()
handleOpenExport fs = do
  now <- liftIO getZonedTime
  let name = "export-" <> toText (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now)
  examPhase .= openExportDialog name fs

handleExportInput :: V.Event -> ExportingData -> TuiM ()
handleExportInput ev ed = case F.focusGetCurrent (ed ^. exportDialog . exportFocus) of
  Just ExportFilenameEditor -> do
    editor' <-
      liftEvent $
        nestEventM' (ed ^. exportDialog . exportEditor) (E.handleEditorEvent (VtyEvent ev))
    examPhase .= Exporting (ed & exportDialog . exportEditor .~ editor')
  Just ExportFormatChooser -> do
    formats' <-
      liftEvent $
        nestEventM'
          (ed ^. exportDialog . exportFormats)
          (L.handleListEventVi L.handleListEvent ev)
    examPhase .= Exporting (ed & exportDialog . exportFormats .~ formats')
  _ -> pass

handleExportConfirm :: ExportingData -> TuiM ()
handleExportConfirm ed = do
  let dlg = ed ^. exportDialog
      name = exportBaseName dlg
  when (name /= "") $ do
    res <-
      liftIO . try $
        writeExport
          (toString name)
          (selectedExportFormat dlg)
          (toExportInput (ed ^. exportFinished))
    let msg = case res of
          Right path -> "Exported to " <> toText path
          Left (e :: IOException) -> "Export failed: " <> show e
    examPhase .= finishExport msg ed
