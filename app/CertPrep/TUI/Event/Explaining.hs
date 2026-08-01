module CertPrep.TUI.Event.Explaining (
  explainingDispatcher,
  explainScroll,
  requestExplanationFor,
) where

import Brick
import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Lens.Micro.Mtl (use, (+=), (.=))
import Prelude hiding (Down)

import CertPrep.Exam
import CertPrep.Explanations (MonadExplain (..))
import CertPrep.TUI.Event.Core (mkDispatcher)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiM,
  liftEvent,
  whenExplaining,
 )

explainingDispatcher :: KeyDispatcher KeyEvent TuiM
explainingDispatcher =
  mkDispatcher
    [ onEvent NextAnswerEvent "Scroll Down" (liftEvent $ vScrollBy explainScroll 1),
      onEvent PreviousAnswerEvent "Scroll Up" (liftEvent $ vScrollBy explainScroll (-1)),
      onEvent ScrollUpEvent "Scroll Up" (liftEvent $ vScrollPage explainScroll Up),
      onEvent ScrollDownEvent "Scroll Down" (liftEvent $ vScrollPage explainScroll Down),
      onEvent DismissExplanationEvent "Back to Review" (whenExplaining handleExplaining)
    ]

-- Brick drops scroll requests for viewports that aren't rendered, so these
-- are safe to issue in any phase; they only take effect while explaining.
explainScroll :: ViewportScroll Name
explainScroll = viewportScroll ExplanationViewport

handleExplaining :: ActivePhase ExplainingData -> TuiM ()
handleExplaining ap = examPhase .= backToReview ap

requestExplanationFor ::
  (MonadState AppState m, MonadExplain m) => ActivePhase ReviewingData -> m ()
requestExplanationFor ap = do
  enabled <- explainAvailable
  when enabled $ do
    rid <- use nextExplainId
    nextExplainId += 1
    let (req, phase) = beginExplanation rid ap
    examPhase .= phase -- Pending shown immediately, before any network I/O
    requestExplanation req
