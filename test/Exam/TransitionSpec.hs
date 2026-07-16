module Exam.TransitionSpec (spec) where

import CertPrep.Exam.Core
import CertPrep.Exam.Transition (applyExplainEvent, stepExplanation, travelToQuestion)
import CertPrep.Explanations (ExplainError (..), ExplainEvent (..), renderExplainError)
import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Generators (mkQuestion)
import Lens.Micro
import Test.Hspec

spec :: Spec
spec = do
    let qs =
            [ mkQuestion "Q1" ["A", "B", "C"] [0] Nothing
            , mkQuestion "Q2" ["X", "Y"] [1] Nothing
            , mkQuestion "Q3" ["M", "N"] [0] Nothing
            ]
        mkCore idx scr =
            ExamCore
                { _questions = V.fromList qs
                , _currentIndex = idx
                , _score = scr
                , _elapsedSeconds = 42
                , _questionStartTime = 0
                , _userAnswers = V.fromList [IS.fromList [0], IS.fromList [1]]
                }
        mkReviewing idx =
            ActivePhase
                { _activeCore = mkCore idx 0
                , _activeQuestion = qs !! idx
                , _phaseData =
                    ReviewingData
                        { _answerResult = undefined
                        , _lastSelected = IS.empty
                        }
                }
    describe "travelToQuestion" $ do
        it "travels to previous answered question" $
            let ap = travelToQuestion (-1) (mkReviewing 1)
             in (ap ^. (activeCore . currentIndex)) `shouldBe` 0
        it "travels to next answered question" $
            let ap = travelToQuestion 1 (mkReviewing 0)
             in (ap ^. (activeCore . currentIndex)) `shouldBe` 1
        it "doesn't travel beyond the last answered question" $
            let ap = travelToQuestion 1 (mkReviewing 1)
             in (ap ^. (activeCore . currentIndex)) `shouldBe` 1
        it "doesn't travel beyond the first answered question" $
            let ap = travelToQuestion (-1) (mkReviewing 0)
             in (ap ^. (activeCore . currentIndex)) `shouldBe` 0

    describe "stepExplanation" $ do
        it "starts streaming on the first chunk" $
            stepExplanation (ExplainChunk "Hel") ExplanationPending
                `shouldBe` ExplanationStreaming "Hel"
        it "appends subsequent chunks" $
            stepExplanation (ExplainChunk "lo") (ExplanationStreaming "Hel")
                `shouldBe` ExplanationStreaming "Hello"
        it "finalizes the streamed text on done" $
            stepExplanation ExplainDone (ExplanationStreaming "Hello")
                `shouldBe` ExplanationSuccess "Hello"
        it "treats done without any chunk as an empty response" $
            stepExplanation ExplainDone ExplanationPending
                `shouldBe` ExplanationFailure (renderExplainError ExplainEmptyResponse)
        it "fails mid-stream on error, discarding partial text" $
            stepExplanation (ExplainFailed (ExplainHttpError "boom")) (ExplanationStreaming "Hel")
                `shouldBe` ExplanationFailure (renderExplainError (ExplainHttpError "boom"))
        it "ignores chunks after success" $
            stepExplanation (ExplainChunk "junk") (ExplanationSuccess "Hello")
                `shouldBe` ExplanationSuccess "Hello"
        it "ignores done after failure" $
            stepExplanation ExplainDone (ExplanationFailure "nope")
                `shouldBe` ExplanationFailure "nope"

    describe "applyExplainEvent" $ do
        let mkExplaining rid status =
                Explaining
                    ActivePhase
                        { _activeCore = mkCore 0 0
                        , _activeQuestion = head qs
                        , _phaseData =
                            ExplainingData
                                { _explainId = rid
                                , _explanationStatus = status
                                , _reviewingData =
                                    ReviewingData
                                        { _answerResult = undefined
                                        , _lastSelected = IS.empty
                                        }
                                }
                        }
            statusOf (Explaining ap) = Just (ap ^. phaseData . explanationStatus)
            statusOf _ = Nothing
        it "applies events carrying the current request id" $
            statusOf (applyExplainEvent 1 (ExplainChunk "Hi") (mkExplaining 1 ExplanationPending))
                `shouldBe` Just (ExplanationStreaming "Hi")
        it "drops events from an abandoned request" $
            statusOf (applyExplainEvent 1 (ExplainChunk "Hi") (mkExplaining 2 ExplanationPending))
                `shouldBe` Just ExplanationPending
        it "leaves other phases untouched" $
            statusOf (applyExplainEvent 1 ExplainDone (Reviewing (mkReviewing 0)))
                `shouldBe` Nothing
