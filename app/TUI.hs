module TUI (
    runApp,
    selectConfig,
)
where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.List.NonEmpty (NonEmpty)
import Exam.Core (AppState, Name)
import Exam.Transition (initialState)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import TUI.Attributes (theMap)
import TUI.ConfigSelect (selectConfig)
import TUI.Draw (drawUI)
import TUI.Event (CustomEvent (..), handleEvent)
import Trophy (EarnedTrophies)
import Types (Question)

app :: App AppState CustomEvent Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

runApp :: NonEmpty Question -> FilePath -> EarnedTrophies -> IO AppState
runApp sampledQuestionsNE canonPath earned = do
    chan <- newBChan 10
    void $ forkIO $ forever $ do
        threadDelay 1000000
        writeBChan chan Tick

    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    customMain
        initialVty
        buildVty
        (Just chan)
        app
        (initialState sampledQuestionsNE canonPath earned)
