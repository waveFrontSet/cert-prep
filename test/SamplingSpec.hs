module SamplingSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Generators (largeQuestionsWithCategories, mkQuestion, questionsWithCategories)
import Sampling (SamplingStrategy (..), sampleQuestions)
import System.Random (mkStdGen)
import Test.Hspec
import Test.QuickCheck
import Types (Question (..))

spec :: Spec
spec = do
    describe "Uniform sampling" $ do
        it "result length = min(n, bank size) when n > 0" $
            property $ \(Positive n) ->
                forAll (listOf1 arbitrary) $ \qs ->
                    let result = sampleQuestions (mkStdGen 42) n Uniform qs
                     in length result === min n (length qs)

        it "all results are from the original bank" $
            property $ \(Positive n) ->
                forAll (listOf1 arbitrary) $ \qs ->
                    let result = sampleQuestions (mkStdGen 42) n Uniform qs
                     in all (`elem` qs) result === True

        it "different seeds can produce different orderings" $
            property $
                forAll (vectorOf 10 arbitrary) $ \qs ->
                    let n = length qs - 1
                        r1 = sampleQuestions (mkStdGen 1) n Uniform qs
                        r2 = sampleQuestions (mkStdGen 2) n Uniform qs
                     in n >= 2 ==> r1 /= r2

        it "n = 0 yields empty" $
            property $ \qs ->
                sampleQuestions (mkStdGen 42) 0 Uniform qs === []

        it "n < 0 yields empty" $
            property $ \(Positive n) qs ->
                sampleQuestions (mkStdGen 42) (negate n) Uniform qs === []

        it "n >= bank size yields entire bank" $
            property $
                forAll (listOf1 arbitrary) $ \qs ->
                    let n = length qs + 10
                        result = sampleQuestions (mkStdGen 42) n Uniform qs
                     in length result === length qs

        it "empty question list returns empty" $
            property $ \(Positive n) ->
                sampleQuestions (mkStdGen 42) n Uniform [] === ([] :: [Question])

        it "same seed always produces same result" $
            property $ \(Positive n) ->
                forAll (listOf1 arbitrary) $ \qs ->
                    let r1 = sampleQuestions (mkStdGen 42) n Uniform qs
                        r2 = sampleQuestions (mkStdGen 42) n Uniform qs
                     in r1 === r2

        it "n exactly equals bank size returns entire bank unchanged" $
            property $
                forAll (listOf1 arbitrary) $ \qs ->
                    let result = sampleQuestions (mkStdGen 42) (length qs) Uniform qs
                     in result === qs

    describe "Stratified sampling" $ do
        let cats = ["AWS Storage", "AWS Compute", "AWS Networking"]
            weights = Map.fromList [("AWS Storage", 2), ("AWS Compute", 1), ("AWS Networking", 1)]

        it "result length = min(n, total matching questions)" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let matching = length $ filter (hasWeightedCategory weights) qs
                        result =
                            sampleQuestions
                                (mkStdGen 42)
                                n
                                (Stratified weights)
                                qs
                     in length result === min n matching

        it "all results belong to weighted categories" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let result =
                            sampleQuestions
                                (mkStdGen 42)
                                n
                                (Stratified weights)
                                qs
                     in all (hasWeightedCategory weights) result === True

        it "different seeds can produce different stratified orderings" $
            property $
                forAll (largeQuestionsWithCategories cats) $ \qs ->
                    let n = min 5 (length qs)
                        r1 = sampleQuestions (mkStdGen 1) n (Stratified weights) qs
                        r2 = sampleQuestions (mkStdGen 2) n (Stratified weights) qs
                     in r1 /= r2 || n <= 1

        it "questions not in weight map are excluded" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories (cats ++ ["Unweighted"])) $ \qs ->
                    let narrowWeights = Map.fromList [("AWS Storage", 1)]
                        result =
                            sampleQuestions
                                (mkStdGen 42)
                                n
                                (Stratified narrowWeights)
                                qs
                     in all
                            (\q -> category q == Just "AWS Storage")
                            result
                            === True

        it "empty weights map returns empty" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    sampleQuestions (mkStdGen 42) n (Stratified Map.empty) qs === []

        it "no matching categories returns empty" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let noMatchWeights = Map.fromList [("Nonexistent", 1)]
                     in sampleQuestions (mkStdGen 42) n (Stratified noMatchWeights) qs === []

        it "same seed always produces same stratified result" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let r1 = sampleQuestions (mkStdGen 42) n (Stratified weights) qs
                        r2 = sampleQuestions (mkStdGen 42) n (Stratified weights) qs
                     in r1 === r2

        it "questions with Nothing category are excluded" $ do
            let noCatQs =
                    [ mkQuestion "NC1" ["A", "B"] [0] Nothing
                    , mkQuestion "NC2" ["A", "B"] [1] Nothing
                    ]
                catQs =
                    [ mkQuestion "C1" ["A", "B"] [0] (Just "AWS Storage")
                    , mkQuestion "C2" ["A", "B"] [1] (Just "AWS Compute")
                    ]
                allQs = noCatQs ++ catQs
                result = sampleQuestions (mkStdGen 42) 10 (Stratified weights) allQs
            all (isJust . category) result `shouldBe` True

        it "single category in weights" $ do
            let singleWeight = Map.fromList [("AWS Storage", 1)]
                qs =
                    [ mkQuestion "S1" ["A"] [0] (Just "AWS Storage")
                    , mkQuestion "S2" ["B"] [0] (Just "AWS Storage")
                    , mkQuestion "C1" ["C"] [0] (Just "AWS Compute")
                    ]
                result = sampleQuestions (mkStdGen 42) 3 (Stratified singleWeight) qs
            length result `shouldBe` 2
            all (\q -> category q == Just "AWS Storage") result `shouldBe` True

        it "per-category counts approximate weight proportions" $
            property $
                forAll (largeQuestionsWithCategories cats) $ \qs ->
                    let perCatAvail =
                            Map.fromListWith (+) $
                                [ (c, 1 :: Int)
                                | q <- qs
                                , Just c <- [category q]
                                , Map.member c weights
                                ]
                        -- Only test when every weighted category has
                        -- enough questions
                        minAvail = minimum $ Map.elems perCatAvail
                     in minAvail >= 4 ==>
                            forAll (chooseInt (length cats, minAvail * length cats `div` 2)) $
                                \n ->
                                    let result =
                                            sampleQuestions
                                                (mkStdGen 42)
                                                n
                                                (Stratified weights)
                                                qs
                                        totalWeight = sum weights
                                        resultLen = length result
                                        countCat c =
                                            length $
                                                filter
                                                    (\q -> category q == Just c)
                                                    result
                                     in conjoin
                                            [ let ideal =
                                                    fromIntegral resultLen
                                                        * fromIntegral
                                                            ( Map.findWithDefault
                                                                0
                                                                c
                                                                weights
                                                            )
                                                        / fromIntegral totalWeight ::
                                                        Double
                                                  actual = countCat c
                                               in counterexample
                                                    ( show c
                                                        ++ ": expected ~"
                                                        ++ show ideal
                                                        ++ " got "
                                                        ++ show actual
                                                    )
                                                    $ property
                                                    $ fromIntegral actual
                                                        >= (ideal - 1.0 :: Double)
                                                        && fromIntegral actual
                                                            <= (ideal + 1.0 :: Double)
                                            | c <- cats
                                            ]

hasWeightedCategory :: Map.Map Text Int -> Question -> Bool
hasWeightedCategory weights q = case category q of
    Just c -> Map.member c weights
    Nothing -> False
