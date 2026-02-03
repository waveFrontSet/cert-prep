module Main (main) where

import Data.Aeson
import Data.IntSet (fromList)
import Test.Hspec
import Types

main :: IO ()
main = hspec $ do
    let q = Question{questionText = "What is 1 + 1?", questionAnswerChoices = ["1", "2", "3"], questionCorrectAnswer = fromList [1], questionCategory = Nothing}
    describe "JSON parsing" $ do
        let rawQ = "{\"answerChoices\":[\"1\",\"2\",\"3\"],\"correctAnswer\":[1],\"text\":\"What is 1 + 1?\"}"
            rawQWithCat = "{\"answerChoices\":[\"1\",\"2\",\"3\"],\"category\":null,\"correctAnswer\":[1],\"text\":\"What is 1 + 1?\"}"
        it "should parse a question" $ do
            decode rawQ `shouldBe` Just q
        it "should encode a question" $ do
            encode q `shouldBe` rawQWithCat
    describe "Eval Answers" $ do
        let q' = q{questionCorrectAnswer = fromList [1, 2]}
        it "should check correct answers" $ do
            isCorrect q (fromList [1]) `shouldBe` True
            isCorrect q (fromList [2]) `shouldBe` False
        it "should correctly report missing and wrong answers" $ do
            evalAnswer q' (fromList [0, 1]) `shouldBe` AnswerResult{answerResultCorrect = fromList [1], answerResultMissing = fromList [2], answerResultWrong = fromList [0]}
