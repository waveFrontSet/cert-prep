module TUI.ConfigSelect (selectConfig) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List qualified as L
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Registry (Registry, RegistryEntry (..))

data SelectName = SelectList
    deriving (Show, Eq, Ord)

newtype SelectState = SelectState
    { _selectList :: L.List SelectName RegistryEntry
    }

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
        title = T.unpack (registryEntryTitle entry)
        path = registryEntryPath entry
        time =
            formatTime
                defaultTimeLocale
                "%Y-%m-%d %H:%M"
                (registryEntryLastUsed entry)
     in hBox
            [ str marker
            , str " "
            , str title
            , padLeft Max $ str (time ++ "  " ++ path)
            ]

handleSelectEvent ::
    BrickEvent SelectName e -> EventM SelectName SelectState ()
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleSelectEvent (VtyEvent (Vty.EvKey Vty.KEnter [])) = halt
handleSelectEvent (VtyEvent e) = do
    SelectState l <- get
    l' <- nestEventM' l (L.handleListEvent e)
    put (SelectState l')
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
    pure (registryEntryPath <$> mSelected)
