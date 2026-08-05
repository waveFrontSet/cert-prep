module CertPrep.TUI.Draw.Export (drawExportDialog) where

import Brick
import Brick.Forms (Form, renderForm)
import Brick.Widgets.Center (centerLayer, hCenter)

import CertPrep.Exam.Core
import CertPrep.TUI.Draw.Core

drawExportDialog :: Form ExportDialogState CustomEvent Name -> Widget Name
drawExportDialog dlg =
  centerLayer $
    panelWithTitle (txt "Export Report") $
      hLimit 46 $
        vBox
          [ renderForm dlg,
            padTop (Pad 1) $ hCenter $ txt "[Enter] Save  [Tab] Switch  [Esc] Cancel"
          ]
