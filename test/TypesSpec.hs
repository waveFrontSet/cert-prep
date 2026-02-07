module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.IntSet (fromList)
import Data.IntSet qualified as IS
import Generators (mkQuestion)
import Test.Hspec
import Test.QuickCheck
import Types

spec :: Spec
spec = do
    let q = mkQuestion "What is 1 + 1?" ["1", "2", "3"] [1] Nothing

    describe "Question JSON" $ do
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

    describe "Config JSON" $ do
        it "roundtrips Config through JSON" $
            property $ \(c :: Config) ->
                decode (encode c) === Just c
        it "parses config with category weights" $ do
            let raw = "{\"questions\":[],\"sampleAmount\":10,\"categoryWeights\":{\"AWS Storage\":2}}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config with weights"
                Just c -> do
                    configSampleAmount c `shouldBe` 10
                    configQuestions c `shouldBe` []
                    configCategoryWeights c `shouldNotBe` Nothing
        it "parses config without category weights" $ do
            let raw = "{\"questions\":[],\"sampleAmount\":5}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config without weights"
                Just c -> do
                    configSampleAmount c `shouldBe` 5
                    configCategoryWeights c `shouldBe` Nothing

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
        it "returns all correct when answer matches exactly" $ do
            let result = evalAnswer q' (fromList [1, 2])
            answerResultCorrect result `shouldBe` fromList [1, 2]
            answerResultMissing result `shouldBe` IS.empty
            answerResultWrong result `shouldBe` IS.empty
        it "returns all missing when answer is empty" $ do
            let result = evalAnswer q' IS.empty
            answerResultCorrect result `shouldBe` IS.empty
            answerResultMissing result `shouldBe` fromList [1, 2]
            answerResultWrong result `shouldBe` IS.empty
        it "returns all wrong when no correct answers selected" $ do
            let result = evalAnswer q' (fromList [0, 3])
            answerResultCorrect result `shouldBe` IS.empty
            answerResultMissing result `shouldBe` fromList [1, 2]
            answerResultWrong result `shouldBe` fromList [0, 3]
        it "handles question with empty correct answer set" $ do
            let emptyQ = q{questionCorrectAnswer = IS.empty}
                result = evalAnswer emptyQ (fromList [0, 1])
            answerResultCorrect result `shouldBe` IS.empty
            answerResultMissing result `shouldBe` IS.empty
            answerResultWrong result `shouldBe` fromList [0, 1]
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
