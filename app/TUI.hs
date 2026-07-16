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
import Exam.Core (AppState)
import Exam.Transition (initialState)
import Explanations (ExplainEnv)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import TUI.Attributes (theMap)
import TUI.ConfigSelect (selectConfig)
import TUI.Draw (drawUI)
import TUI.Event (CustomEvent (..), handleEvent)
import TUI.Monad (TuiEnv (..), runTuiM)
import Trophy (EarnedTrophies)
import Types (Question)

runApp ::
    FilePath -> Maybe ExplainEnv -> NonEmpty Question -> EarnedTrophies -> IO AppState
runApp p mExplainEnv qs earned = do
    chan <- newBChan 10
    void $ forkIO $ forever $ do
        threadDelay 1000000
        writeBChan chan Tick

    let buildVty = mkVty V.defaultConfig
        env = TuiEnv{tuiConfigPath = p, tuiEventChan = chan, tuiExplainEnv = mExplainEnv}
        app =
            App
                { appDraw = drawUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent = runTuiM env . handleEvent
                , appStartEvent = return ()
                , appAttrMap = const theMap
                }
    initialVty <- buildVty
    customMain
        initialVty
        buildVty
        (Just chan)
        app
        (initialState qs earned)
