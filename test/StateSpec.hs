module StateSpec (spec) where

import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Generators (mkQuestion)
import Lens.Micro ((^.))
import State
import Test.Hspec
import Types (defaultTrophies)

spec :: Spec
spec = do
    let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
        q2 = mkQuestion "Q2" ["X", "Y"] [1] (Just "AWS Storage")
        q3 = mkQuestion "Q3" ["P", "Q", "R", "S"] [0, 2] (Just "AWS Compute")
        qs = NE.fromList [q1, q2, q3]

    let unwrapAnswering :: ExamPhase -> ActivePhase AnsweringData
        unwrapAnswering (Answering ap) = ap
        unwrapAnswering _ = error "expected Answering phase"

    describe "initialState" $ do
        it "returns an Answering phase" $
            case initialState qs defaultTrophies of
                Answering _ -> True
                _ -> False
                `shouldBe` True
        it "sets currentIndex to 0" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. activeCore . currentIndex `shouldBe` 0
        it "sets selectedAnswers to empty" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. phaseData . selectedAnswers `shouldBe` IS.empty
        it "sets focusedAnswer to 0" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. phaseData . focusedAnswer `shouldBe` 0
        it "sets score to 0" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. activeCore . score `shouldBe` 0
        it "sets elapsedSeconds to 0" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. activeCore . elapsedSeconds `shouldBe` 0
        it "stores all questions" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. activeCore . questions `shouldBe` V.fromList [q1, q2, q3]
        it "preserves question order" $
            let ap =
                    unwrapAnswering
                        (initialState (NE.fromList [q3, q1, q2]) defaultTrophies)
             in ap ^. activeCore . questions `shouldBe` V.fromList [q3, q1, q2]
        it "initializes available trophies from config" $
            let ap = unwrapAnswering (initialState qs defaultTrophies)
             in ap ^. activeCore . availableTrophies `shouldBe` defaultTrophies

    describe "totalQuestions" $ do
        let ap = unwrapAnswering (initialState qs defaultTrophies)
            core = ap ^. activeCore
        it "returns the length of the question list" $
            totalQuestions core `shouldBe` 3
