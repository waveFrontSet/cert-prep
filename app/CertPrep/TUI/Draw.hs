module CertPrep.TUI.Draw (drawUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import CertPrep.Exam.Core
import CertPrep.TUI.Attributes
import CertPrep.TUI.Trophy (drawTrophyAwarded)
import CertPrep.Types (AnswerResult (..), Question (..))
import CertPrep.Util (formatTime)
import Data.IntSet qualified as IS
import Data.Text qualified as T
import Lens.Micro ((^.))

drawUI :: AppState -> [Widget Name]
drawUI appState = case appState ^. examPhase of
    Finished fs -> [drawFinished fs]
    Answering ap ->
        [ drawExam
            (ap ^. activeCore)
            (ap ^. activeQuestion)
            (drawAnswerAnswering ap)
            drawStatusAnswering
        ]
    Reviewing ap ->
        [ drawExam
            (ap ^. activeCore)
            (ap ^. activeQuestion)
            (drawAnswerReviewing ap)
            drawStatusReviewing
        ]
    Explaining ap ->
        [ drawExam
            (ap ^. activeCore)
            (ap ^. activeQuestion)
            (drawExplanation ap)
            drawStatusReviewing
        ]
    CheckingTrophies _ -> [emptyWidget]
    TrophyAwarded tad -> [drawTrophyAwarded tad]

drawFinished :: FinishedState -> Widget Name
drawFinished fs =
    withBorderStyle unicode $
        borderWithLabel (txt " Exam Complete ") $
            center $
                vBox
                    [ txt ""
                    , hCenter $ txt "Your Results"
                    , txt ""
                    , hCenter $ txt $ "Score: " <> show (fs ^. finalScore) <> " / " <> show (fs ^. finalTotal)
                    , hCenter $ txt $ "Percentage: " <> show percentage <> "%"
                    , txt ""
                    , hCenter $ txt $ "Total time: " <> formatTime (fs ^. finalElapsed)
                    , hCenter $ txt $ "Avg per question: " <> formatTime avgTime
                    , txt ""
                    , hCenter $ txt "Press 'q' or Esc to exit"
                    , txt ""
                    ]
  where
    percentage :: Int
    percentage =
        if fs ^. finalTotal == 0
            then 0
            else (fs ^. finalScore * 100) `div` (fs ^. finalTotal)
    avgTime :: Int
    avgTime =
        if fs ^. finalTotal == 0
            then 0
            else (fs ^. finalElapsed + fs ^. finalTotal - 1) `div` (fs ^. finalTotal)

drawExam ::
    ExamCore -> Question -> (Int -> Text -> Widget Name) -> Widget Name -> Widget Name
drawExam core q drawAnswer statusButton =
    vBox
        [ hBox
            [ questionPanel
            , vBorder
            , answersPanel
            ]
        , hBorder
        , statusBar
        ]
  where
    panelWithTitle :: Text -> Widget Name -> Widget Name
    panelWithTitle title body =
        withBorderStyle unicode $ borderWithLabel (padLeftRight 1 $ txt title) $ padAll 1 body
    questionPanel = panelWithTitle (renderQuestionTitle core) $ txtWrap (text q)
    answersPanel =
        panelWithTitle
            "Answers"
            (vBox $ zipWith drawAnswer [0 ..] (answerChoices q))

    statusBar =
        padLeftRight 1 $
            vLimitPercent 10 $
                hBox
                    [ txt $ renderScoreStatus core
                    , padLeft (Pad 1) (txt $ renderTimeStatus core)
                    , hCenter statusButton
                    , txt $ renderKeybindings core
                    ]

renderQuestionTitle :: ExamCore -> Text
renderQuestionTitle core =
    T.intercalate
        " "
        ["Question", show (core ^. currentIndex + 1), "of", show (totalQuestions core)]

renderScoreStatus :: ExamCore -> Text
renderScoreStatus core = T.intercalate " " ["Score:", show (core ^. score), "/", show (totalQuestions core)]

renderTimeStatus :: ExamCore -> Text
renderTimeStatus core = T.intercalate " " ["Time:", formatTime (core ^. elapsedSeconds)]

renderKeybindings :: ExamCore -> Text
renderKeybindings _ = "[q] Quit  [Space] Toggle  [Arrow Keys] Navigate"

drawAnswerAnswering :: ActivePhase AnsweringData -> Int -> Text -> Widget Name
drawAnswerAnswering ap idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            applyFocus $
                hBox [checkbox, padLeft (Pad 1) wrappedText]
  where
    selected = IS.member idx (ap ^. phaseData . selectedAnswers)
    focused = ap ^. phaseData . focusedAnswer == idx

    applyFocus w = if focused then withAttr focusedAttr w else w
    wrappedText = txtWrap answerText
    checkbox =
        if selected
            then withAttr selectedAttr $ txt "[X]"
            else txt "[ ]"

drawAnswerReviewing :: ActivePhase ReviewingData -> Int -> Text -> Widget Name
drawAnswerReviewing ap idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            hBox [checkbox, padLeft (Pad 1) answerWidget]
  where
    result = ap ^. phaseData . answerResult
    isCorrectSelection = IS.member idx (correct result)
    isMissed = IS.member idx (missing result)
    isWrong = IS.member idx (wrong result)

    wrappedText = txtWrap answerText

    checkbox
        | isCorrectSelection = withAttr correctAttr $ txt "✅"
        | isMissed = withAttr missedAttr $ txt "⚠️"
        | isWrong = withAttr wrongAttr $ txt "❌"
        | otherwise = txt "[ ]"

    answerWidget
        | isCorrectSelection = withAttr correctAttr wrappedText
        | isMissed = withAttr missedAttr wrappedText
        | isWrong = withAttr wrongAttr wrappedText
        | otherwise = wrappedText

drawExplanation :: ActivePhase ExplainingData -> Int -> Text -> Widget Name
drawExplanation ap 0 _ = viewport ExplanationViewport Vertical explainText
  where
    eStatus = ap ^. phaseData . explanationStatus
    explainText = case eStatus of
        ExplanationPending -> txtWrap "Generating explanation…"
        ExplanationStreaming t -> txtWrap (t <> "▌")
        ExplanationSuccess t -> txtWrap t
        ExplanationFailure t -> withAttr wrongAttr $ txtWrap (t <> "\n\nPress Enter to return.")
drawExplanation _ _ _ = emptyWidget

drawStatusAnswering :: Widget Name
drawStatusAnswering =
    clickable SubmitButton $
        withAttr submitAttr $
            padLeftRight 1 $
                txt "[Enter] Submit"

drawStatusReviewing :: Widget Name
drawStatusReviewing =
    clickable NextButton $
        withAttr nextAttr $
            padLeftRight 1 $
                txt "[Enter] Next"
