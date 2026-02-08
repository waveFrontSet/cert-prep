module EventSpec (spec) where

import Data.IntSet qualified as IS
import Test.Hspec
import TUI.Event (moveFocusPure, toggleAnswerPure)

spec :: Spec
spec = do
    describe "toggleAnswerPure" $ do
        it "inserts index into empty set" $
            toggleAnswerPure 2 IS.empty `shouldBe` IS.fromList [2]
        it "removes index if already present" $
            toggleAnswerPure 2 (IS.fromList [1, 2, 3]) `shouldBe` IS.fromList [1, 3]
        it "inserts index if not present" $
            toggleAnswerPure 4 (IS.fromList [1, 2]) `shouldBe` IS.fromList [1, 2, 4]
        it "toggling twice returns original set" $
            let sel = IS.fromList [1, 3]
             in toggleAnswerPure 2 (toggleAnswerPure 2 sel) `shouldBe` sel

    describe "moveFocusPure" $ do
        it "wraps forward past last answer" $
            moveFocusPure 1 4 5 `shouldBe` 0
        it "wraps backward past first answer" $
            moveFocusPure (-1) 0 5 `shouldBe` 4
        it "moves forward normally" $
            moveFocusPure 1 2 5 `shouldBe` 3
        it "moves backward normally" $
            moveFocusPure (-1) 3 5 `shouldBe` 2
        it "returns current when numAnswers is 0" $
            moveFocusPure 1 0 0 `shouldBe` 0
        it "returns current when numAnswers is negative" $
            moveFocusPure 1 0 (-1) `shouldBe` 0
        it "handles single answer" $
            moveFocusPure 1 0 1 `shouldBe` 0
