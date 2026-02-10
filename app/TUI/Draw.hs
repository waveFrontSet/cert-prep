module TUI.Draw (drawUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.IntSet qualified as IS
import Data.Text (Text)
import Lens.Micro ((^.))
import State
import TUI.Attributes
import Types (AnswerResult (..), Question (..))
import Util (formatTime)

drawUI :: ExamPhase -> [Widget Name]
drawUI (Finished fs) = [drawFinished fs]
drawUI (Answering ap) =
    [ drawExam
        (ap ^. activeCore)
        (ap ^. activeQuestion)
        (drawAnswerAnswering ap)
        drawStatusAnswering
    ]
drawUI (Reviewing ap) =
    [ drawExam
        (ap ^. activeCore)
        (ap ^. activeQuestion)
        (drawAnswerReviewing ap)
        drawStatusReviewing
    ]

drawFinished :: FinishedState -> Widget Name
drawFinished fs =
    withBorderStyle unicode $
        borderWithLabel (str " Exam Complete ") $
            center $
                vBox
                    [ str ""
                    , hCenter $ str "Your Results"
                    , str ""
                    , hCenter $ str $ "Score: " ++ show (fs ^. finalScore) ++ " / " ++ show (fs ^. finalTotal)
                    , hCenter $ str $ "Percentage: " ++ show percentage ++ "%"
                    , str ""
                    , hCenter $ str $ "Total time: " ++ formatTime (fs ^. finalElapsed)
                    , hCenter $ str $ "Avg per question: " ++ formatTime avgTime
                    , str ""
                    , hCenter $ str "Press 'q' or Esc to exit"
                    , str ""
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
    questionPanel =
        withBorderStyle unicode
            $ borderWithLabel
                ( str $
                    " Question "
                        ++ show (core ^. currentIndex + 1)
                        ++ " of "
                        ++ show (totalQuestions core)
                        ++ " "
                )
            $ padAll 1
            $ txtWrap (text q)

    answersPanel =
        withBorderStyle unicode $
            borderWithLabel (str " Answers ") $
                padAll 1 $
                    vBox $
                        zipWith drawAnswer [0 ..] (answerChoices q)

    statusBar =
        padLeftRight 1 $
            vLimitPercent 10 $
                hBox
                    [ str $ "Score: " ++ show (core ^. score) ++ "/" ++ show (totalQuestions core)
                    , str $ "  Time: " ++ formatTime (core ^. elapsedSeconds)
                    , fill ' '
                    , statusButton
                    , fill ' '
                    , str "[q] Quit  [Space] Toggle  [Arrow Keys] Navigate"
                    ]

drawAnswerAnswering :: ActivePhase AnsweringData -> Int -> Text -> Widget Name
drawAnswerAnswering ap idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            applyFocus $
                hBox [checkbox, str " ", wrappedText]
  where
    selected = IS.member idx (ap ^. phaseData . selectedAnswers)
    focused = ap ^. phaseData . focusedAnswer == idx

    applyFocus w = if focused then withAttr focusedAttr w else w
    wrappedText = txtWrap answerText
    checkbox =
        if selected
            then withAttr selectedAttr $ str "[X]"
            else str "[ ]"

drawAnswerReviewing :: ActivePhase ReviewingData -> Int -> Text -> Widget Name
drawAnswerReviewing ap idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            hBox [checkbox, str " ", answerWidget]
  where
    result = ap ^. phaseData . answerResult
    isCorrectSelection = IS.member idx (correct result)
    isMissed = IS.member idx (missing result)
    isWrong = IS.member idx (wrong result)

    wrappedText = txtWrap answerText

    checkbox
        | isCorrectSelection = withAttr correctAttr $ str "[+]"
        | isMissed = withAttr missedAttr $ str "[O]"
        | isWrong = withAttr wrongAttr $ str "[X]"
        | otherwise = str "[ ]"

    answerWidget
        | isCorrectSelection = withAttr correctAttr wrappedText
        | isMissed = withAttr missedAttr wrappedText
        | isWrong = withAttr wrongAttr wrappedText
        | otherwise = wrappedText

drawStatusAnswering :: Widget Name
drawStatusAnswering =
    clickable SubmitButton $
        withAttr submitAttr $
            str " [Enter] Submit "

drawStatusReviewing :: Widget Name
drawStatusReviewing =
    clickable NextButton $
        withAttr nextAttr $
            str " [Enter] Next "
