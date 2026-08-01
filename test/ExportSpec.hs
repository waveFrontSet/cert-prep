{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ExportSpec (spec) where

import Data.Aeson (Value, decode)
import Data.Map qualified as M
import Data.Text qualified as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import CertPrep.Export
import CertPrep.Export.Core
import CertPrep.Types (Answer, Question)
import Generators (arbitraryQAPair, correctlyAnsweredQAPair, mkQuestion)

spec :: Spec
spec = do
  describe "toReport" $ do
    prop "counts correctly answered questions" $
      onReport (inputOf correctlyAnsweredQAPair) $ \input report ->
        report.totalCorrect === length input.qaPairs
    prop "correct answers are lte total questions" $
      onReport (inputOf arbitraryQAPair) $ \_ report ->
        property $ report.totalCorrect <= report.totalQuestions
    prop "counts every question" $
      onReport (inputOf arbitraryQAPair) $ \input report ->
        report.totalQuestions === length input.qaPairs
    prop "forwards elapsed seconds" $
      onReport (inputOf arbitraryQAPair) $ \input report ->
        report.elapsedSeconds === input.elapsedSeconds
    prop "recovers qa pairs" $
      onReport (inputOf arbitraryQAPair) $ \input report ->
        input.qaPairs === fmap (\r -> (r.question, r.answer)) report.questionResults
    prop "category totals sum to total questions" $
      onReport (inputOf arbitraryQAPair) $ \_ report ->
        sum (fmap (.total) (M.elems report.categoryStats)) === report.totalQuestions
    prop "category corrects sum to total correct" $
      onReport (inputOf arbitraryQAPair) $ \_ report ->
        sum (fmap (.correct) (M.elems report.categoryStats)) === report.totalCorrect
  describe "jsonExporter" $
    prop "produces parseable JSON" $
      forAll (inputOf arbitraryQAPair) $ \input ->
        decode @Value (export Json input) =/= Nothing
  describe "markdownExporter" $ do
    it "renders the report in the final format" $
      markdownOf goldenInput `shouldBe` goldenOutput
    prop "renders one section per question" $
      forAll (inputOf arbitraryQAPair) $ \input ->
        length (T.breakOnAll "### Question " (markdownOf input))
          === length input.qaPairs
    prop "lists every category in the table" $
      onReport (inputOf arbitraryQAPair) $ \input report ->
        conjoin
          [ property $
              ("| " <> fromMaybe "Uncategorized" cat <> " |") `T.isInfixOf` markdownOf input
          | cat <- M.keys report.categoryStats
          ]
    prop "always starts with the report title" $
      forAll (inputOf arbitraryQAPair) $ \input ->
        property $ "# Certification Exam Report\n" `T.isPrefixOf` markdownOf input
  describe "fileExtension" $ do
    it "is md for Markdown" $
      fileExtension Markdown `shouldBe` "md"
    it "is json for Json" $
      fileExtension Json `shouldBe` "json"
  describe "writeExport" $
    it "writes the report with the format's extension" $
      withSystemTempDirectory "cert-prep-test" $ \dir -> do
        path <- writeExport (dir </> "report") Markdown goldenInput
        path `shouldBe` dir </> "report.md"
        contents <- decodeUtf8 <$> readFileLBS path
        contents `shouldBe` goldenOutput

markdownOf :: ExportInput -> Text
markdownOf = decodeUtf8 . export Markdown

goldenInput :: ExportInput
goldenInput =
  ExportInput {
    qaPairs =
      [ ( mkQuestion "What is S3?" ["Object storage", "Block storage"] [0] (Just "AWS Storage"),
          fromList [0]
        ),
        ( mkQuestion
            "What does EC2 provide?"
            ["Object storage", "Virtual servers", "DNS routing"]
            [1]
            (Just "AWS Compute"),
          fromList [0]
        ),
        ( mkQuestion "Which are AWS services?" ["EC2", "Excel", "S3"] [0, 2] Nothing,
          fromList [0, 2]
        )
      ],
    elapsedSeconds = 754
  }

goldenOutput :: Text
goldenOutput =
  unlines
    [ "# Certification Exam Report",
      "",
      "**Score:** 2 / 3 (67%)",
      "**Time:** 12:34",
      "",
      "## Results by Category",
      "",
      "| Category | Correct | Score |",
      "| --- | --- | --- |",
      "| AWS Compute | 0 / 1 | 0% |",
      "| AWS Storage | 1 / 1 | 100% |",
      "| Uncategorized | 1 / 1 | 100% |",
      "",
      "## Questions",
      "",
      "### Question 1 (AWS Storage) — ✓ Correct",
      "",
      "What is S3?",
      "",
      "1. Object storage",
      "2. Block storage",
      "",
      "**Your answer:** 1",
      "**Correct answer:** 1",
      "",
      "### Question 2 (AWS Compute) — ✗ Incorrect",
      "",
      "What does EC2 provide?",
      "",
      "1. Object storage",
      "2. Virtual servers",
      "3. DNS routing",
      "",
      "**Your answer:** 1",
      "**Correct answer:** 2",
      "",
      "### Question 3 — ✓ Correct",
      "",
      "Which are AWS services?",
      "",
      "1. EC2",
      "2. Excel",
      "3. S3",
      "",
      "**Your answer:** 1, 3",
      "**Correct answer:** 1, 3"
    ]

inputOf :: Gen (Question, Answer) -> Gen ExportInput
inputOf genPair = do
  pairs <- listOf genPair
  secs <- chooseInt (0, 7200)
  pure ExportInput {qaPairs = pairs, elapsedSeconds = secs}

onReport :: Gen ExportInput -> (ExportInput -> ExportReport -> Property) -> Property
onReport gen f = forAll gen $ \input -> f input (toReport input)
