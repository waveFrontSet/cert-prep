module CertPrep.TUI.Event.Reviewing (
  reviewingDispatcher,
  handleNextQuestion,
) where

import Brick (vScrollToBeginning)
import Brick.Keybindings (
  KeyDispatcher,
  onEvent,
 )
import Lens.Micro ((^.))
import Lens.Micro.Mtl ((.=))

import CertPrep.Exam
import CertPrep.TUI.Event.Core (mkDispatcher)
import CertPrep.TUI.Event.Explaining (explainScroll, requestExplanationFor)
import CertPrep.TUI.Event.Trophy (handleCheckTrophies)
import CertPrep.TUI.Keybindings (KeyEvent (..))
import CertPrep.TUI.Monad (
  TuiM,
  liftEvent,
  whenReviewing,
 )

reviewingDispatcher :: KeyDispatcher KeyEvent TuiM
reviewingDispatcher =
  mkDispatcher
    [ onEvent
        ReviewNextQuestionEvent
        "Select Next Question"
        (modifyReviewing (travelToQuestion 1)),
      onEvent
        ReviewPreviousQuestionEvent
        "Select Previous Question"
        (modifyReviewing (travelToQuestion (-1))),
      onEvent FinishReviewEvent "Finish Review" (whenReviewing handleNextQuestion),
      onEvent
        RequestAiExplanationEvent
        "Request AI Explanation"
        ( whenReviewing $ \ap -> do
            liftEvent $ vScrollToBeginning explainScroll
            requestExplanationFor ap
        )
    ]

handleNextQuestion :: ActivePhase ReviewingData -> TuiM ()
handleNextQuestion ap = handleCheckTrophies (ap ^. activeCore)

modifyReviewing :: (ActivePhase ReviewingData -> ActivePhase ReviewingData) -> TuiM ()
modifyReviewing f = whenReviewing (\ap -> examPhase .= Reviewing (f ap))
