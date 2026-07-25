module ExplanationsSpec (spec) where

import Data.IntSet qualified as IS
import Test.Hspec

import CertPrep.Explanations (renderExplainPrompt)
import CertPrep.Types
import Generators (mkQuestion)

spec :: Spec
spec = do
  let q = mkQuestion "What is 1 + 1?" ["1", "2", "3"] [1] Nothing
      a = AnswerResult {correct = IS.fromList [1], missing = IS.empty, wrong = IS.empty}
  describe "Explanations" $ do
    it "should render a prompt" $ do
      renderExplainPrompt q a
        `shouldBe` "Question: What is 1 + 1?\n\
                   \Answer Choices:\n\
                   \0. 1\n\
                   \1. 2\n\
                   \2. 3\n\
                   \Correct Answers: 1\n\
                   \User Selected Answers: 1"
