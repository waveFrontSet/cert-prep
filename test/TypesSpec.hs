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
        it "rejects JSON missing required fields" $ do
            let noText = "{\"answerChoices\":[\"A\"],\"correctAnswer\":[0]}"
                noChoices = "{\"text\":\"Q\",\"correctAnswer\":[0]}"
                noAnswer = "{\"text\":\"Q\",\"answerChoices\":[\"A\"]}"
            (decode noText :: Maybe Question) `shouldBe` Nothing
            (decode noChoices :: Maybe Question) `shouldBe` Nothing
            (decode noAnswer :: Maybe Question) `shouldBe` Nothing
        it "rejects JSON with wrong field types" $ do
            let textAsInt = "{\"text\":42,\"answerChoices\":[\"A\"],\"correctAnswer\":[0]}"
                choicesAsStr = "{\"text\":\"Q\",\"answerChoices\":\"not-a-list\",\"correctAnswer\":[0]}"
            (decode textAsInt :: Maybe Question) `shouldBe` Nothing
            (decode choicesAsStr :: Maybe Question) `shouldBe` Nothing
        it "rejects completely invalid JSON" $ do
            (decode "not json at all" :: Maybe Question) `shouldBe` Nothing
            (decode "" :: Maybe Question) `shouldBe` Nothing
        it "accepts extra fields gracefully" $ do
            let withExtra = "{\"text\":\"Q\",\"answerChoices\":[\"A\"],\"correctAnswer\":[0],\"bonus\":true}"
            case decode withExtra :: Maybe Question of
                Nothing -> expectationFailure "Should accept JSON with extra fields"
                Just q' -> text q' `shouldBe` "Q"

    describe "Config JSON" $ do
        it "roundtrips Config through JSON" $
            property $ \(c :: Config) ->
                decode (encode c) === Just c
        it "parses config with category weights" $ do
            let raw =
                    "{\"title\":\"Test\",\"questions\":[],\"sampleAmount\":10,\"categoryWeights\":{\"AWS Storage\":2}}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config with weights"
                Just c -> do
                    sampleAmount c `shouldBe` 10
                    questions c `shouldBe` []
                    categoryWeights c `shouldNotBe` Nothing
        it "parses config without category weights" $ do
            let raw = "{\"title\":\"Test\",\"questions\":[],\"sampleAmount\":5}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config without weights"
                Just c -> do
                    sampleAmount c `shouldBe` 5
                    categoryWeights c `shouldBe` Nothing
        it "rejects config missing required fields" $ do
            let noQuestions = "{\"title\":\"T\",\"sampleAmount\":5}"
                noSampleAmount = "{\"title\":\"T\",\"questions\":[]}"
                noTitle = "{\"questions\":[],\"sampleAmount\":5}"
            (decode noQuestions :: Maybe Config) `shouldBe` Nothing
            (decode noSampleAmount :: Maybe Config) `shouldBe` Nothing
            (decode noTitle :: Maybe Config) `shouldBe` Nothing
        it "parses config with zero sample amount" $ do
            let raw = "{\"title\":\"Test\",\"questions\":[],\"sampleAmount\":0}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config with sampleAmount 0"
                Just c -> sampleAmount c `shouldBe` 0
        it "parses config with negative sample amount" $ do
            let raw = "{\"title\":\"Test\",\"questions\":[],\"sampleAmount\":-5}"
            case decode raw :: Maybe Config of
                Nothing -> expectationFailure "Failed to parse config with negative sampleAmount"
                Just c -> sampleAmount c `shouldBe` (-5)

    describe "Eval Answers" $ do
        let q' = q{correctAnswer = fromList [1, 2]}
        it "should check correct answers" $ do
            isCorrect q (fromList [1]) `shouldBe` True
            isCorrect q (fromList [2]) `shouldBe` False
        it "should correctly report missing and wrong answers" $ do
            evalAnswer q' (fromList [0, 1])
                `shouldBe` AnswerResult
                    { correct = fromList [1]
                    , missing = fromList [2]
                    , wrong = fromList [0]
                    }
        it "returns all correct when answer matches exactly" $ do
            let result = evalAnswer q' (fromList [1, 2])
            correct result `shouldBe` fromList [1, 2]
            missing result `shouldBe` IS.empty
            wrong result `shouldBe` IS.empty
        it "returns all missing when answer is empty" $ do
            let result = evalAnswer q' IS.empty
            correct result `shouldBe` IS.empty
            missing result `shouldBe` fromList [1, 2]
            wrong result `shouldBe` IS.empty
        it "returns all wrong when no correct answers selected" $ do
            let result = evalAnswer q' (fromList [0, 3])
            correct result `shouldBe` IS.empty
            missing result `shouldBe` fromList [1, 2]
            wrong result `shouldBe` fromList [0, 3]
        it "handles question with empty correct answer set" $ do
            let emptyQ = q{correctAnswer = IS.empty}
                result = evalAnswer emptyQ (fromList [0, 1])
            correct result `shouldBe` IS.empty
            missing result `shouldBe` IS.empty
            wrong result `shouldBe` fromList [0, 1]
        it "isCorrect with both empty correct set and empty answer" $ do
            let emptyQ = q{correctAnswer = IS.empty}
            isCorrect emptyQ IS.empty `shouldBe` True
        it "handles question with zero answer choices" $ do
            let noChoicesQ = mkQuestion "Empty?" [] [] Nothing
                result = evalAnswer noChoicesQ IS.empty
            correct result `shouldBe` IS.empty
            missing result `shouldBe` IS.empty
            wrong result `shouldBe` IS.empty
            isCorrect noChoicesQ IS.empty `shouldBe` True
        it "partitions into disjoint sets covering all relevant indices" $
            property $ \(q'' :: Question) ->
                let numChoices = length (answerChoices q'')
                 in forAll (sublistOf [0 .. numChoices - 1]) $ \selected ->
                        let ans = IS.fromList selected
                            result = evalAnswer q'' ans
                            c = correct result
                            m = missing result
                            w = wrong result
                         in conjoin
                                [ IS.intersection c m === IS.empty
                                , IS.intersection c w === IS.empty
                                , IS.intersection m w === IS.empty
                                , IS.union c m
                                    === correctAnswer q''
                                , IS.union c w === ans
                                ]
        it "isCorrect iff evalAnswer has empty missing and wrong" $
            property $ \(q'' :: Question) ->
                let numChoices = length (answerChoices q'')
                 in forAll (sublistOf [0 .. numChoices - 1]) $ \selected ->
                        let ans = IS.fromList selected
                            result = evalAnswer q'' ans
                            m = missing result
                            w = wrong result
                         in isCorrect q'' ans
                                === (IS.null m && IS.null w)
