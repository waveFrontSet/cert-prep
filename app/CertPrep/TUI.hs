module CertPrep.TUI (
    runApp,
    selectConfig,
)
where

import Brick
import Brick.BChan (newBChan, writeBChan)
import CertPrep.Exam.Core (AppState)
import CertPrep.Exam.Transition (initialState)
import CertPrep.Explanations (ExplainEnv)
import CertPrep.TUI.Attributes (theMap)
import CertPrep.TUI.ConfigSelect (selectConfig)
import CertPrep.TUI.Draw (drawUI)
import CertPrep.TUI.Event (CustomEvent (..), handleEvent)
import CertPrep.TUI.Monad (TuiEnv (..), runTuiM)
import CertPrep.Trophy (EarnedTrophies)
import CertPrep.Types (Question)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.List.NonEmpty (NonEmpty)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)

runApp ::
    FilePath -> Maybe ExplainEnv -> NonEmpty Question -> EarnedTrophies -> IO AppState
runApp p mExplainEnv qs earned = do
    -- writeBChan blocks when full; token chunks arrive in bursts and share
    -- this channel with the timer ticks, so leave generous headroom.
    chan <- newBChan 100
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
