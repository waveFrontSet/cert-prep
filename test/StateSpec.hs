module StateSpec (spec) where

import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Vector qualified as V
import Generators (mkQuestion)
import Lens.Micro ((^.))
import State
import Test.Hspec

spec :: Spec
spec = do
    let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
        q2 = mkQuestion "Q2" ["X", "Y"] [1] (Just "AWS Storage")
        q3 = mkQuestion "Q3" ["P", "Q", "R", "S"] [0, 2] (Just "AWS Compute")
        qs = NE.fromList [q1, q2, q3]
        appState0 = initialState qs "/test/config.json" Set.empty

    let unwrapAnswering :: ExamPhase -> ActivePhase AnsweringData
        unwrapAnswering (Answering ap) = ap
        unwrapAnswering _ = error "expected Answering phase"

    describe "initialState" $ do
        it "returns an Answering phase" $
            case appState0 ^. examPhase of
                Answering _ -> True
                _ -> False
                `shouldBe` True
        it "sets currentIndex to 0" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. activeCore . currentIndex `shouldBe` 0
        it "sets selectedAnswers to empty" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. phaseData . selectedAnswers `shouldBe` IS.empty
        it "sets focusedAnswer to 0" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. phaseData . focusedAnswer `shouldBe` 0
        it "sets score to 0" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. activeCore . score `shouldBe` 0
        it "sets elapsedSeconds to 0" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. activeCore . elapsedSeconds `shouldBe` 0
        it "sets questionStartTime to 0" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. activeCore . questionStartTime `shouldBe` 0
        it "stores all questions" $
            let ap = unwrapAnswering (appState0 ^. examPhase)
             in ap ^. activeCore . questions `shouldBe` V.fromList [q1, q2, q3]
        it "preserves question order" $
            let as' = initialState (NE.fromList [q3, q1, q2]) "/test" Set.empty
                ap = unwrapAnswering (as' ^. examPhase)
             in ap ^. activeCore . questions `shouldBe` V.fromList [q3, q1, q2]
        it "initializes currentStreak to 0" $
            appState0 ^. currentStreak `shouldBe` 0
        it "stores the config path" $
            appState0 ^. configPath `shouldBe` "/test/config.json"
        it "stores earned trophies" $
            appState0 ^. earnedTrophies `shouldBe` Set.empty

    describe "totalQuestions" $ do
        let ap = unwrapAnswering (appState0 ^. examPhase)
            core = ap ^. activeCore
        it "returns the length of the question list" $
            totalQuestions core `shouldBe` 3
