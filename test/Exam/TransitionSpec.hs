module Exam.TransitionSpec (spec) where

import Data.IntSet qualified as IS
import Data.Vector qualified as V
import Exam.Core
import Exam.Transition (travelToQuestion)
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
