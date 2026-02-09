{-# LANGUAGE TemplateHaskell #-}

module TUI.ConfigSelect (selectConfig) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List qualified as L
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro.TH (makeLenses)
import Registry (Registry, RegistryEntry (..))

data SelectName = SelectList
    deriving (Show, Eq, Ord)

newtype SelectState = SelectState
    { _selectList :: L.List SelectName RegistryEntry
    }

makeLenses ''SelectState

drawSelectUI :: SelectState -> [Widget SelectName]
drawSelectUI (SelectState l) = [ui]
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

renderEntry :: Bool -> RegistryEntry -> Widget SelectName
renderEntry selected entry =
    let marker = if selected then ">" else " "
        p = path entry
        time =
            formatTime
                defaultTimeLocale
                "%Y-%m-%d %H:%M"
                (lastUsed entry)
     in hBox
            [ str marker
            , str " "
            , txt $ title entry
            , padLeft Max $ str (time ++ "  " ++ p)
            ]

handleSelectEvent ::
    BrickEvent SelectName e -> EventM SelectName SelectState ()
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey (Vty.KChar c) [])) = case c of
    'j' -> handleSelectEvent (VtyEvent (Vty.EvKey Vty.KDown []))
    'k' -> handleSelectEvent (VtyEvent (Vty.EvKey Vty.KUp []))
    'q' -> halt
    _ -> return ()
handleSelectEvent (VtyEvent e) = do
    zoom selectList $ L.handleListEvent e
handleSelectEvent _ = return ()

selectConfig :: Registry -> IO (Maybe FilePath)
selectConfig entries = do
    let list =
            L.list
                SelectList
                (V.fromList entries)
                1
        initial = SelectState list
        app =
            App
                { appDraw = drawSelectUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent = handleSelectEvent
                , appStartEvent = return ()
                , appAttrMap =
                    const $
                        attrMap
                            Vty.defAttr
                            [(L.listSelectedFocusedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)]
                }
    let buildVty = mkVty Vty.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty Nothing app initial
    let mSelected = snd <$> L.listSelectedElement (_selectList finalState)
    pure (path <$> mSelected)
