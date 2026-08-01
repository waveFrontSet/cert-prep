module CertPrep.TUI.Keybindings where

import Brick.Keybindings (
  Binding,
  KeyConfig,
  KeyEvents,
  bind,
  ctrl,
  keyEvents,
  newKeyConfig,
 )
import Graphics.Vty qualified as Vty

data KeyEvent
  = PreviousQuestionBankEvent
  | NextQuestionBankEvent
  | SelectQuestionBankEvent
  | ToggleSelectedAnswerEvent
  | NextAnswerEvent
  | PreviousAnswerEvent
  | SubmitAnswersEvent
  | AcceptTrophyEvent
  | ReviewNextQuestionEvent
  | ReviewPreviousQuestionEvent
  | FinishReviewEvent
  | RequestAiExplanationEvent
  | DismissExplanationEvent
  | ContinueEvent
  | OpenExportDialogEvent
  | ExportConfirmEvent
  | ExportCancelEvent
  | ExportSwitchFieldEvent
  | ScrollUpEvent
  | ScrollDownEvent
  | QuitEvent
  deriving (Eq, Show, Ord, Enum)

allEvents :: KeyEvents KeyEvent
allEvents =
  keyEvents
    [ ("quit", QuitEvent),
      ("prev-question-bank", PreviousQuestionBankEvent),
      ("next-question-bank", NextQuestionBankEvent),
      ("select-question-bank", SelectQuestionBankEvent),
      ("toggle-selected-answer", ToggleSelectedAnswerEvent),
      ("next-answer", NextAnswerEvent),
      ("previous-answer", PreviousAnswerEvent),
      ("submit-answers", SubmitAnswersEvent),
      ("accept-trophy", AcceptTrophyEvent),
      ("review-next-question", ReviewNextQuestionEvent),
      ("review-previous-question", ReviewPreviousQuestionEvent),
      ("finish-review", FinishReviewEvent),
      ("request-ai-explanation", RequestAiExplanationEvent),
      ("dismiss-explanation", DismissExplanationEvent),
      ("continue", ContinueEvent),
      ("open-export-dialog", OpenExportDialogEvent),
      ("export-confirm", ExportConfirmEvent),
      ("export-cancel", ExportCancelEvent),
      ("export-switch-field", ExportSwitchFieldEvent),
      ("scroll-up", ScrollUpEvent),
      ("scroll-down", ScrollDownEvent)
    ]

defaultKeybindings :: [(KeyEvent, [Binding])]
defaultKeybindings =
  [ (QuitEvent, [bind 'q']),
    (PreviousQuestionBankEvent, [bind 'k', bind Vty.KUp]),
    (NextQuestionBankEvent, [bind 'j', bind Vty.KDown]),
    (SelectQuestionBankEvent, [bind Vty.KEnter]),
    (ToggleSelectedAnswerEvent, [bind ' ']),
    (NextAnswerEvent, [bind 'j', bind Vty.KDown]),
    (PreviousAnswerEvent, [bind 'k', bind Vty.KUp]),
    (SubmitAnswersEvent, [bind Vty.KEnter]),
    (AcceptTrophyEvent, [bind Vty.KEnter]),
    (ReviewNextQuestionEvent, [bind 'l']),
    (ReviewPreviousQuestionEvent, [bind 'h']),
    (FinishReviewEvent, [bind Vty.KEnter]),
    (RequestAiExplanationEvent, [bind 'a']),
    (DismissExplanationEvent, [bind Vty.KEnter]),
    (ContinueEvent, [bind Vty.KEnter]),
    (OpenExportDialogEvent, [bind 'e']),
    (ExportConfirmEvent, [bind Vty.KEnter]),
    (ExportCancelEvent, [bind Vty.KEsc]),
    (ExportSwitchFieldEvent, [bind '\t']),
    (ScrollUpEvent, [ctrl 'b']),
    (ScrollDownEvent, [ctrl 'f'])
  ]

defaultKeyConfig :: KeyConfig KeyEvent
defaultKeyConfig = newKeyConfig allEvents defaultKeybindings []
