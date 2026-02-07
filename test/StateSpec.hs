module StateSpec (spec) where

import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Generators (mkQuestion)
import State
import Test.Hspec

spec :: Spec
spec = do
    let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
        q2 = mkQuestion "Q2" ["X", "Y"] [1] (Just "AWS Storage")
        q3 = mkQuestion "Q3" ["P", "Q", "R", "S"] [0, 2] (Just "AWS Compute")
        qs = [q1, q2, q3]

    describe "initialState" $ do
        it "sets currentIndex to 0" $
            _currentIndex (initialState qs) `shouldBe` 0
        it "sets selectedAnswers to empty" $
            _selectedAnswers (initialState qs) `shouldBe` IS.empty
        it "sets focusedAnswer to 0" $
            _focusedAnswer (initialState qs) `shouldBe` 0
        it "sets phase to Answering" $
            _phase (initialState qs) `shouldBe` Answering
        it "sets score to 0" $
            _score (initialState qs) `shouldBe` 0
        it "sets elapsedSeconds to 0" $
            _elapsedSeconds (initialState qs) `shouldBe` 0
        it "stores all questions" $
            _questions (initialState qs) `shouldBe` V.fromList qs
        it "handles empty question list" $
            _questions (initialState []) `shouldBe` V.empty
        it "preserves question order" $
            _questions (initialState [q3, q1, q2]) `shouldBe` V.fromList [q3, q1, q2]

    describe "currentQuestion" $ do
        it "returns the first question at index 0" $
            currentQuestion (initialState qs) `shouldBe` Just q1
        it "returns Nothing when index is out of bounds" $
            currentQuestion ((initialState qs){_currentIndex = 10}) `shouldBe` Nothing
        it "returns Nothing for empty question list" $
            currentQuestion (initialState []) `shouldBe` Nothing
        it "returns correct question at valid index" $
            currentQuestion ((initialState qs){_currentIndex = 2}) `shouldBe` Just q3
        it "returns Nothing for negative index" $
            currentQuestion ((initialState qs){_currentIndex = -1}) `shouldBe` Nothing
        it "returns Nothing at index exactly equal to length" $
            currentQuestion ((initialState qs){_currentIndex = 3}) `shouldBe` Nothing

    describe "totalQuestions" $ do
        it "returns the length of the question list" $
            totalQuestions (initialState qs) `shouldBe` 3
        it "returns 0 for empty list" $
            totalQuestions (initialState []) `shouldBe` 0
