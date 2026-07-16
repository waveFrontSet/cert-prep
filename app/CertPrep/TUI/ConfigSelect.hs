module CertPrep.TUI.ConfigSelect (selectConfig) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List qualified as L
import CertPrep.Registry (Registry, RegistryEntry (..))
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty

type SelectState = L.List () RegistryEntry

drawSelectUI :: SelectState -> [Widget ()]
drawSelectUI l = [ui]
  where
    ui =
        withBorderStyle unicode $
            borderWithLabel (str " Select a Config ") $
                center $
                    hLimitPercent 80 $
                        vLimitPercent 80 $
                            vBox
                                [ L.renderList renderEntry True l
                                , hBorder
                                , padLeftRight 1 $
                                    str "[Enter] Select  [q/Esc] Quit  [Arrow Keys] Navigate"
                                ]

renderEntry :: Bool -> RegistryEntry -> Widget ()
renderEntry selected entry =
    let marker = if selected then "→" else " "
        p = path entry
        time =
            formatTime
                defaultTimeLocale
                "%Y-%m-%d %H:%M"
                (lastUsed entry)
     in withAttr (attrName "marker") (str marker)
            <+> vBox
                [ hCenter $ txt (title entry)
                , hCenter $ str (time ++ "  " ++ p)
                ]

handleSelectEvent :: BrickEvent () e -> EventM () SelectState ()
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleSelectEvent (VtyEvent e) = L.handleListEventVi L.handleListEvent e
handleSelectEvent _ = return ()

theMap :: AttrMap
theMap =
    attrMap
        Vty.defAttr
        [ (L.listSelectedFocusedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
        , (attrName "marker", style Vty.bold)
        ]

selectConfig :: Registry -> IO (Maybe FilePath)
selectConfig entries = do
    let initial = L.list () (V.fromList entries) 1
        app =
            App
                { appDraw = drawSelectUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent = handleSelectEvent
                , appStartEvent = return ()
                , appAttrMap = const theMap
                }
    finalState <- defaultMain app initial
    let mSelected = snd <$> L.listSelectedElement finalState
    return $ path <$> mSelected
