{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CertPrep.Export.Markdown (markdownExporter) where

import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Text qualified as T

import CertPrep.Common (formatTime)
import CertPrep.Export.Core
import CertPrep.Types (Answer, Category, Question (..))

markdownExporter :: Exporter
markdownExporter =
  Exporter {
    extension = "md",
    render = encodeUtf8 . renderReport
  }

renderReport :: ExportReport -> Text
renderReport report =
  T.intercalate "\n\n" sections <> "\n"
 where
  sections =
    ["# Certification Exam Report", scoreBlock report]
      <> [categoryTable report.categoryStats | not (M.null report.categoryStats)]
      <> ["## Questions"]
      <> zipWith questionSection [1 ..] report.questionResults

scoreBlock :: ExportReport -> Text
scoreBlock report =
  T.intercalate
    "\n"
    [ "**Score:** "
        <> show report.totalCorrect
        <> " / "
        <> show report.totalQuestions
        <> percentage,
      "**Time:** " <> formatTime report.elapsedSeconds
    ]
 where
  percentage
    | report.totalQuestions == 0 = ""
    | otherwise = " (" <> percent report.totalCorrect report.totalQuestions <> ")"

categoryTable :: Map (Maybe Category) CategoryStat -> Text
categoryTable stats =
  T.intercalate "\n" $
    [ "## Results by Category",
      "",
      "| Category | Correct | Score |",
      "| --- | --- | --- |"
    ]
      -- alphabetical, with the uncategorized bucket last
      <> fmap row (sortOn (isNothing . (.category)) (M.elems stats))
 where
  row cs =
    "| "
      <> fromMaybe "Uncategorized" cs.category
      <> " | "
      <> show cs.correct
      <> " / "
      <> show cs.total
      <> " | "
      <> percent cs.correct cs.total
      <> " |"

questionSection :: Int -> QuestionResult -> Text
questionSection ix qr =
  T.intercalate "\n" $
    [heading, "", qr.question.text, ""]
      <> zipWith choice [1 ..] qr.question.answerChoices
      <> [ "",
           "**Your answer:** " <> showAnswer qr.answer,
           "**Correct answer:** " <> showAnswer qr.question.correctAnswer
         ]
 where
  heading =
    "### Question " <> show ix <> renderCategory qr.question.category <> " — " <> verdict
  renderCategory Nothing = ""
  renderCategory (Just c) = " (" <> c <> ")"
  verdict = if qr.wasCorrect then "✓ Correct" else "✗ Incorrect"
  choice :: Int -> Text -> Text
  choice n c = show n <> ". " <> c

-- | 1-based choice indices, e.g. @fromList [0,2]@ -> @"1, 3"@.
showAnswer :: Answer -> Text
showAnswer ans
  | IS.null ans = "(none)"
  | otherwise = T.intercalate ", " (fmap (show . (+ 1)) (IS.toAscList ans))

percent :: Int -> Int -> Text
percent x total =
  show (round (fromIntegral x / fromIntegral total * 100 :: Double) :: Int) <> "%"
