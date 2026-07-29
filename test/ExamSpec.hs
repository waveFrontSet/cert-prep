module ExamSpec (spec) where

import Lens.Micro ((^.))
import Test.Hspec

import CertPrep.Exam.Core
import CertPrep.Exam.Transition (initialState)
import CertPrep.Trophy (TrophyState (..))
import Generators (mkQuestion)

spec :: Spec
spec = do
  let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
      q2 = mkQuestion "Q2" ["X", "Y"] [1] (Just "AWS Storage")
      q3 = mkQuestion "Q3" ["P", "Q", "R", "S"] [0, 2] (Just "AWS Compute")
      qs = fromList [q1, q2, q3]
      appState0 = initialState qs mempty

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
       in ap ^. phaseData . selectedAnswers `shouldBe` mempty
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
       in ap ^. activeCore . questions `shouldBe` fromList [q1, q2, q3]
    it "preserves question order" $
      let as' = initialState (fromList [q3, q1, q2]) mempty
          ap = unwrapAnswering (as' ^. examPhase)
       in ap ^. activeCore . questions `shouldBe` fromList [q3, q1, q2]
    it "initializes currentStreak to 0" $
      appState0 ^. trophyState
        `shouldBe` TrophyState {currentStreak = 0, lastQuestionSeconds = 0}
    it "stores earned trophies" $
      appState0 ^. earnedTrophies `shouldBe` mempty

  describe "totalQuestions" $ do
    let ap = unwrapAnswering (appState0 ^. examPhase)
        core = ap ^. activeCore
    it "returns the length of the question list" $
      totalQuestions core `shouldBe` 3
