module CertPrep.TUI.Keybindings where

import Brick.Keybindings (Binding, KeyEvents, bind, ctrl, keyEvents)
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
    | NextQuestionEvent
    | PreviousQuestionEvent
    | RequestAiExplanationEvent
    | ScrollUpEvent
    | ScrollDownEvent
    | QuitEvent
    deriving (Eq, Show, Ord, Enum)

allEvents :: KeyEvents KeyEvent
allEvents =
    keyEvents
        [ ("quit", QuitEvent)
        , ("prev-question-bank", PreviousQuestionBankEvent)
        , ("next-question-bank", NextQuestionBankEvent)
        , ("select-question-bank", SelectQuestionBankEvent)
        , ("toggle-selected-answer", ToggleSelectedAnswerEvent)
        , ("next-answer", NextAnswerEvent)
        , ("previous-answer", PreviousAnswerEvent)
        , ("submit-answers", SubmitAnswersEvent)
        , ("accept-trophy", AcceptTrophyEvent)
        , ("next-question", NextQuestionEvent)
        , ("previous-question", PreviousQuestionEvent)
        , ("request-ai-explanation", RequestAiExplanationEvent)
        , ("scroll-up", ScrollUpEvent)
        , ("scroll-down", ScrollDownEvent)
        ]

defaultKeybindings :: [(KeyEvent, [Binding])]
defaultKeybindings =
    [ (QuitEvent, [bind 'q'])
    , (PreviousQuestionBankEvent, [bind 'k', bind Vty.KUp])
    , (NextQuestionBankEvent, [bind 'j', bind Vty.KDown])
    , (SelectQuestionBankEvent, [bind Vty.KEnter])
    , (ToggleSelectedAnswerEvent, [bind ' '])
    , (NextAnswerEvent, [bind 'j', bind Vty.KDown])
    , (PreviousAnswerEvent, [bind 'k', bind Vty.KUp])
    , (SubmitAnswersEvent, [bind Vty.KEnter])
    , (AcceptTrophyEvent, [bind Vty.KEnter])
    , (NextQuestionEvent, [bind 'l', bind Vty.KEnter])
    , (PreviousQuestionEvent, [bind 'h'])
    , (RequestAiExplanationEvent, [bind 'a'])
    , (ScrollUpEvent, [ctrl 'b'])
    , (ScrollDownEvent, [ctrl 'f'])
    ]
