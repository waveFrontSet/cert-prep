module CertPrep.TUI.Draw.Export (drawExportDialog) where

import Brick
import Brick.Forms (Form, renderForm)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center (centerLayer)

import CertPrep.Exam.Core

drawExportDialog :: Form ExportDialogState CustomEvent Name -> Widget Name
drawExportDialog dlg =
  centerLayer $
    withBorderStyle unicode $
      borderWithLabel (txt " Export Report ") $
        padAll 1 $
          hLimit 46 $
            vBox
              [ renderForm dlg,
                txt "",
                txt "[Enter] Save  [Tab] Switch  [Esc] Cancel"
              ]
