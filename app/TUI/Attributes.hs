module TUI.Attributes (
    selectedAttr,
    correctAttr,
    wrongAttr,
    missedAttr,
    submitAttr,
    nextAttr,
    focusedAttr,
    theMap,
)
where

import Brick (AttrMap, AttrName, attrMap, attrName, fg, on)
import Graphics.Vty qualified as V

selectedAttr
    , correctAttr
    , wrongAttr
    , missedAttr
    , submitAttr
    , nextAttr
    , focusedAttr ::
        AttrName
selectedAttr = attrName "selected"
correctAttr = attrName "correct"
wrongAttr = attrName "wrong"
missedAttr = attrName "missed"
submitAttr = attrName "submit"
nextAttr = attrName "next"
focusedAttr = attrName "focused"

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (selectedAttr, V.white `on` V.blue)
        , (correctAttr, fg V.green `V.withStyle` V.bold)
        , (wrongAttr, fg V.red `V.withStyle` V.bold)
        , (missedAttr, fg V.yellow `V.withStyle` V.bold)
        , (submitAttr, V.black `on` V.yellow)
        , (nextAttr, V.black `on` V.cyan)
        , (focusedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        ]
