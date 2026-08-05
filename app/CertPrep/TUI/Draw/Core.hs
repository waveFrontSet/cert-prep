module CertPrep.TUI.Draw.Core (panelWithTitle) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import CertPrep.Exam.Core (Name)

panelWithTitle :: Widget Name -> Widget Name -> Widget Name
panelWithTitle title body =
  withBorderStyle unicode $ borderWithLabel (padLeftRight 1 title) $ padAll 1 body
