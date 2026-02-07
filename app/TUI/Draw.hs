module TUI.Draw (drawUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro ((^.))
import State
import TUI.Attributes
import Types (AnswerResult (..), Question (..), evalAnswer)

drawUI :: AppState -> [Widget Name]
drawUI s = [ui]
  where
    ui = case s ^. phase of
        Finished -> drawFinished s
        _ -> drawExam s

drawFinished :: AppState -> Widget Name
drawFinished s =
    withBorderStyle unicode $
        borderWithLabel (str " Exam Complete ") $
            center $
                vBox
                    [ str ""
                    , hCenter $ str "Your Results"
                    , str ""
                    , hCenter $ str $ "Score: " ++ show (s ^. score) ++ " / " ++ show (totalQuestions s)
                    , hCenter $ str $ "Percentage: " ++ show percentage ++ "%"
                    , str ""
                    , hCenter $ str $ "Total time: " ++ formatTime (s ^. elapsedSeconds)
                    , hCenter $ str $ "Avg per question: " ++ formatTime avgTime
                    , str ""
                    , hCenter $ str "Press 'q' or Esc to exit"
                    , str ""
                    ]
  where
    percentage :: Int
    percentage =
        if totalQuestions s == 0
            then 0
            else (s ^. score * 100) `div` totalQuestions s
    avgTime :: Int
    avgTime =
        if totalQuestions s == 0
            then 0
            else (s ^. elapsedSeconds + totalQuestions s - 1) `div` totalQuestions s

drawExam :: AppState -> Widget Name
drawExam s =
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
    mQuestion = currentQuestion s

    questionPanel =
        withBorderStyle unicode $
            borderWithLabel
                ( str $
                    " Question " ++ show (s ^. currentIndex + 1) ++ " of " ++ show (totalQuestions s) ++ " "
                ) $
                hLimitPercent 50 $
                    padAll 1 $
                        case mQuestion of
                            Nothing -> str "No question"
                            Just q -> strWrap (T.unpack $ questionText q)

    answersPanel =
        withBorderStyle unicode $
            borderWithLabel (str " Answers ") $
                hLimitPercent 50 $
                    padAll 1 $
                        case mQuestion of
                            Nothing -> str "No answers"
                            Just q ->
                                let result = evalAnswer q (s ^. selectedAnswers)
                                 in vBox $ zipWith (drawAnswer s result) [0 ..] (questionAnswerChoices q)

    statusBar =
        padLeftRight 1 $
            hBox
                [ str $ "Score: " ++ show (s ^. score) ++ "/" ++ show (s ^. currentIndex)
                , str $ "  Time: " ++ formatTime (s ^. elapsedSeconds)
                , fill ' '
                , case s ^. phase of
                    Answering ->
                        clickable SubmitButton $
                            withAttr submitAttr $
                                str " [Enter] Submit "
                    Reviewing ->
                        clickable NextButton $
                            withAttr nextAttr $
                                str " [Enter] Next "
                    Finished -> str ""
                , fill ' '
                , str "[q] Quit  [Space] Toggle  [Arrow Keys] Navigate"
                ]

drawAnswer :: AppState -> AnswerResult -> Int -> Text -> Widget Name
drawAnswer s result idx answerText =
    clickable (AnswerChoice idx) $
        padBottom (Pad 1) $
            applyFocus $
                hBox [checkbox, str " ", answerWidget]
  where
    selected = IS.member idx (s ^. selectedAnswers)
    focused = s ^. focusedAnswer == idx && s ^. phase == Answering
    isCorrectSelection = IS.member idx (answerResultCorrect result)
    isMissed = IS.member idx (answerResultMissing result)
    isWrong = IS.member idx (answerResultWrong result)

    applyFocus w = if focused then withAttr focusedAttr w else w

    wrappedText = strWrap (T.unpack answerText)

    checkbox = case s ^. phase of
        Answering ->
            if selected
                then withAttr selectedAttr $ str "[X]"
                else str "[ ]"
        Reviewing
            | isCorrectSelection -> withAttr correctAttr $ str "[+]"
            | isMissed -> withAttr missedAttr $ str "[O]"
            | isWrong -> withAttr wrongAttr $ str "[X]"
            | otherwise -> str "[ ]"
        Finished -> str "[ ]"

    answerWidget = case s ^. phase of
        Reviewing
            | isCorrectSelection -> withAttr correctAttr wrappedText
            | isMissed -> withAttr missedAttr wrappedText
            | isWrong -> withAttr wrongAttr wrappedText
            | otherwise -> wrappedText
        _ -> wrappedText

formatTime :: Int -> String
formatTime totalSecs =
    pad mins ++ ":" ++ pad secs
  where
    mins = totalSecs `div` 60
    secs = totalSecs `mod` 60
    pad n = if n < 10 then "0" ++ show n else show n
