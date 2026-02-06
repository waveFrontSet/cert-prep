module SamplingSpec (spec) where

import Data.Map.Strict qualified as Map
import Generators (largeQuestionsWithCategories, questionsWithCategories)
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

        it "no index sampled twice (result length matches expected)" $
            property $ \(Positive n) ->
                forAll (listOf1 arbitrary) $ \qs ->
                    let result = sampleQuestions (mkStdGen 42) n Uniform qs
                     in length result === min n (length qs)

        it "n = 0 yields empty" $
            property $ \qs ->
                sampleQuestions (mkStdGen 42) 0 Uniform qs === []

        it "n >= bank size yields entire bank" $
            property $
                forAll (listOf1 arbitrary) $ \qs ->
                    let n = length qs + 10
                        result = sampleQuestions (mkStdGen 42) n Uniform qs
                     in length result === length qs

    describe "Stratified sampling" $ do
        let cats = ["AWS Storage", "AWS Compute", "AWS Networking"]
            weights = Map.fromList [("AWS Storage", 2), ("AWS Compute", 1), ("AWS Networking", 1)]

        it "result length = min(n, total matching questions)" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let matching =
                            length $
                                filter
                                    ( \q -> case questionCategory q of
                                        Just c -> Map.member c weights
                                        Nothing -> False
                                    )
                                    qs
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
                     in all
                            ( \q -> case questionCategory q of
                                Just c -> Map.member c weights
                                Nothing -> False
                            )
                            result
                            === True

        it "result length matches expected (no index sampled twice)" $
            property $ \(Positive n) ->
                forAll (questionsWithCategories cats) $ \qs ->
                    let matching =
                            length $
                                filter
                                    ( \q -> case questionCategory q of
                                        Just c -> Map.member c weights
                                        Nothing -> False
                                    )
                                    qs
                        result =
                            sampleQuestions
                                (mkStdGen 42)
                                n
                                (Stratified weights)
                                qs
                     in length result === min n matching

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
                            (\q -> questionCategory q == Just "AWS Storage")
                            result
                            === True

        it "per-category counts approximate weight proportions" $
            property $
                forAll (largeQuestionsWithCategories cats) $ \qs ->
                    let perCatAvail =
                            Map.fromListWith (+) $
                                [ (c, 1 :: Int)
                                | q <- qs
                                , Just c <- [questionCategory q]
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
                                                    (\q -> questionCategory q == Just c)
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
                                                    ( c
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
