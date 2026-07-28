{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module ExportSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import CertPrep.Export
import CertPrep.Types (Answer, Question)
import Generators (arbitraryQAPair, correctlyAnsweredQAPair)

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

inputOf :: Gen (Question, Answer) -> Gen ExportInput
inputOf genPair = do
  pairs <- listOf genPair
  secs <- chooseInt (0, 7200)
  pure ExportInput {qaPairs = pairs, elapsedSeconds = secs}

onReport :: Gen ExportInput -> (ExportInput -> ExportReport -> Property) -> Property
onReport gen f = forAll gen $ \input -> f input (toReport input)
