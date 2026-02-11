module TUI.Trophy (drawTrophyAwarded) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Lens.Micro ((^.))
import State (Name, TrophyAwardedData, animationFrame, awardedTrophy)
import TUI.Attributes (trophyIconAttr, trophySparkleAttr, trophyTitleAttr)
import Trophy (TrophyDef (..))

drawTrophyAwarded :: TrophyAwardedData -> Widget Name
drawTrophyAwarded tad =
    center $
        withBorderStyle unicode $
            borderWithLabel (withAttr trophySparkleAttr $ str " \10022 ") $
                padAll 2 $
                    hLimit 40 $
                        vBox $
                            concat
                                [
                                    [ hCenter $
                                        withAttr trophySparkleAttr $
                                            str
                                                "\10022 \10022 \10022 \10022 \10022 \10022 \10022 \10022 \10022"
                                    , str ""
                                    , hCenter $
                                        withAttr trophyTitleAttr $
                                            str "TROPHY UNLOCKED!"
                                    ]
                                , if frame >= 1
                                    then
                                        [ str ""
                                        , hCenter $
                                            withAttr trophyIconAttr $
                                                vBox (map txt (trophyIcon trophy))
                                        ]
                                    else []
                                , if frame >= 2
                                    then
                                        [ str ""
                                        , hCenter $
                                            withAttr trophyTitleAttr $
                                                txt (trophyName trophy)
                                        ]
                                    else []
                                , if frame >= 3
                                    then
                                        [ str ""
                                        , hCenter $ txt (trophyDesc trophy)
                                        , str ""
                                        , hCenter $ str "Press Enter to continue"
                                        ]
                                    else []
                                ]
  where
    frame = tad ^. animationFrame
    trophy = tad ^. awardedTrophy
