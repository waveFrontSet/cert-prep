module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.IntSet (fromList)
import Data.IntSet qualified as IS
import Generators ()
import Test.Hspec
import Test.QuickCheck
import Types

spec :: Spec
spec = do
    let q =
            Question
                { questionText = "What is 1 + 1?"
                , questionAnswerChoices = ["1", "2", "3"]
                , questionCorrectAnswer = fromList [1]
                , questionCategory = Nothing
                }
    describe "JSON parsing" $ do
        let rawQ =
                "{\"answerChoices\":[\"1\",\"2\",\"3\"],\"correctAnswer\":[1],\"text\":\"What is 1 + 1?\"}"
            rawQWithCat =
                "{\"answerChoices\":[\"1\",\"2\",\"3\"],\"category\":null,\"correctAnswer\":[1],\"text\":\"What is 1 + 1?\"}"
        it "should parse a question" $ do
            decode rawQ `shouldBe` Just q
        it "should encode a question" $ do
            encode q `shouldBe` rawQWithCat
        it "roundtrips Question through JSON" $
            property $ \(q' :: Question) ->
                decode (encode q') === Just q'

    describe "Eval Answers" $ do
        let q' = q{questionCorrectAnswer = fromList [1, 2]}
        it "should check correct answers" $ do
            isCorrect q (fromList [1]) `shouldBe` True
            isCorrect q (fromList [2]) `shouldBe` False
        it "should correctly report missing and wrong answers" $ do
            evalAnswer q' (fromList [0, 1])
                `shouldBe` AnswerResult
                    { answerResultCorrect = fromList [1]
                    , answerResultMissing = fromList [2]
                    , answerResultWrong = fromList [0]
                    }
        it "partitions into disjoint sets covering all relevant indices" $
            property $ \(q'' :: Question) ->
                let numChoices = length (questionAnswerChoices q'')
                 in forAll (sublistOf [0 .. numChoices - 1]) $ \selected ->
                        let ans = IS.fromList selected
                            result = evalAnswer q'' ans
                            c = answerResultCorrect result
                            m = answerResultMissing result
                            w = answerResultWrong result
                         in conjoin
                                [ IS.intersection c m === IS.empty
                                , IS.intersection c w === IS.empty
                                , IS.intersection m w === IS.empty
                                , IS.union c m
                                    === questionCorrectAnswer q''
                                , IS.union c w === ans
                                ]
        it "isCorrect iff evalAnswer has empty missing and wrong" $
            property $ \(q'' :: Question) ->
                let numChoices = length (questionAnswerChoices q'')
                 in forAll (sublistOf [0 .. numChoices - 1]) $ \selected ->
                        let ans = IS.fromList selected
                            result = evalAnswer q'' ans
                            m = answerResultMissing result
                            w = answerResultWrong result
                         in isCorrect q'' ans
                                === (IS.null m && IS.null w)
