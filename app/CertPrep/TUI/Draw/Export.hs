module CertPrep.TUI.Draw.Export (drawExportDialog) where

import Brick
import Brick.Focus qualified as F
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Lens.Micro (Lens', (^.))

import CertPrep.Exam.Core
import CertPrep.Exam.Transition (selectedExportFormat)
import CertPrep.Export (ExportFormat (..), fileExtension)

drawExportDialog :: ExportDialogState -> Widget Name
drawExportDialog dlg =
  centerLayer $
    withBorderStyle unicode $
      borderWithLabel (txt " Export Report ") $
        padAll 1 $
          hLimit 46 $
            vBox
              [ txt "Format:",
                vLimit 2 $ withFocus (L.renderList renderFormat) exportFormats,
                txt "",
                hBox
                  [ txt "Filename: ",
                    vLimit 1 $ withFocus (E.renderEditor (txt . mconcat)) exportEditor,
                    txt $ " ." <> fileExtension (selectedExportFormat dlg)
                  ],
                txt "",
                txt "[Enter] Save  [Tab] Switch  [Esc] Cancel"
              ]
 where
  withFocus ::
    (Named a Name) => (Bool -> a -> Widget Name) -> Lens' ExportDialogState a -> Widget Name
  withFocus render l = F.withFocusRing (dlg ^. exportFocus) render (dlg ^. l)
  renderFormat _ fmt = txt (formatLabel fmt)
  formatLabel Markdown = "Markdown"
  formatLabel Json = "JSON"
