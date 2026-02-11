module EventSpec (spec) where

import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Generators (mkQuestion)
import Lens.Micro ((^.))
import State
import TUI.Event (moveFocusPure, nextQuestion, submitAnswer, toggleAnswerPure)
import Test.Hspec

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

    describe "submitAnswer" $ do
        let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
            q2 = mkQuestion "Q2" ["X", "Y"] [1] Nothing
            mkAnswering qs idx sel =
                ActivePhase
                    { _activeCore =
                        ExamCore
                            { _questions = V.fromList qs
                            , _currentIndex = idx
                            , _score = 0
                            , _elapsedSeconds = 0
                            , _questionStartTime = 0
                            }
                    , _activeQuestion = qs !! idx
                    , _phaseData =
                        AnsweringData
                            { _selectedAnswers = sel
                            , _focusedAnswer = 0
                            }
                    }
        it "transitions to Reviewing" $
            case submitAnswer (mkAnswering [q1, q2] 0 (IS.fromList [0])) of
                Reviewing _ -> True
                _ -> False
                `shouldBe` True
        it "increments score for correct answer" $
            case submitAnswer (mkAnswering [q1, q2] 0 (IS.fromList [0])) of
                Reviewing ap -> ap ^. activeCore . score `shouldBe` 1
                _ -> expectationFailure "expected Reviewing"
        it "does not increment score for wrong answer" $
            case submitAnswer (mkAnswering [q1, q2] 0 (IS.fromList [1])) of
                Reviewing ap -> ap ^. activeCore . score `shouldBe` 0
                _ -> expectationFailure "expected Reviewing"

    describe "nextQuestion" $ do
        let q1 = mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
            q2 = mkQuestion "Q2" ["X", "Y"] [1] Nothing
            mkReviewing qs idx scr =
                ActivePhase
                    { _activeCore =
                        ExamCore
                            { _questions = V.fromList qs
                            , _currentIndex = idx
                            , _score = scr
                            , _elapsedSeconds = 42
                            , _questionStartTime = 0
                            }
                    , _activeQuestion = qs !! idx
                    , _phaseData =
                        ReviewingData
                            { _answerResult = undefined
                            , _lastSelected = IS.empty
                            }
                    }
        it "transitions to Answering when more questions remain" $
            case nextQuestion (mkReviewing [q1, q2] 0 1) of
                Answering _ -> True
                _ -> False
                `shouldBe` True
        it "advances currentIndex" $
            case nextQuestion (mkReviewing [q1, q2] 0 1) of
                Answering ap -> ap ^. activeCore . currentIndex `shouldBe` 1
                _ -> expectationFailure "expected Answering"
        it "resets selectedAnswers" $
            case nextQuestion (mkReviewing [q1, q2] 0 1) of
                Answering ap -> ap ^. phaseData . selectedAnswers `shouldBe` IS.empty
                _ -> expectationFailure "expected Answering"
        it "transitions to Finished on last question" $
            case nextQuestion (mkReviewing [q1, q2] 1 2) of
                Finished _ -> True
                _ -> False
                `shouldBe` True
        it "carries score into Finished" $
            case nextQuestion (mkReviewing [q1, q2] 1 2) of
                Finished fs -> fs ^. finalScore `shouldBe` 2
                _ -> expectationFailure "expected Finished"
        it "carries elapsed time into Finished" $
            case nextQuestion (mkReviewing [q1, q2] 1 2) of
                Finished fs -> fs ^. finalElapsed `shouldBe` 42
                _ -> expectationFailure "expected Finished"
