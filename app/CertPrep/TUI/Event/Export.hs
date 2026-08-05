module CertPrep.TUI.Event.Export (
  exportingDispatcher,
  handleOpenExport,
  handleExportInput,
) where

import Brick (BrickEvent (VtyEvent), nestEventM')
import Brick.Forms (Form (formState), handleFormEvent)
import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Control.Exception (IOException, try)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Graphics.Vty qualified as V
import Lens.Micro ((.~), (^.))
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
        (whenExporting (\ed -> examPhase .= cancelExport ed))
    ]

handleOpenExport :: FinishedState -> TuiM ()
handleOpenExport fs = do
  now <- liftIO getZonedTime
  let name = "export-" <> toText (formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now)
  examPhase .= openExportDialog name fs

handleExportInput :: V.Event -> ExportingData -> TuiM ()
handleExportInput ev ed = do
  form <- liftEvent $ nestEventM' (ed ^. exportDialog) (handleFormEvent (VtyEvent ev))
  examPhase .= Exporting (ed & exportDialog .~ form)

handleExportConfirm :: ExportingData -> TuiM ()
handleExportConfirm ed = do
  let dialogstate = formState $ ed ^. exportDialog
      name = dialogstate ^. exportFilename
      format = dialogstate ^. exportFormat
  when (name /= "") $ do
    res <-
      liftIO . try $
        writeExport (toString name) format (toExportInput (ed ^. exportFinished))
    let msg = case res of
          Right path -> "Exported to " <> toText path
          Left (e :: IOException) -> "Export failed: " <> show e
    examPhase .= finishExport msg ed
