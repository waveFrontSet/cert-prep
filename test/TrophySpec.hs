module TrophySpec (spec) where

import Data.Set qualified as Set
import System.Environment (setEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Trophy

spec :: Spec
spec = do
    describe "checkAfterSubmit" $ do
        it "awards FirstBlood for first correct answer" $
            checkAfterSubmit True (TrophyState 1 10)
                `shouldSatisfy` any ((== FirstBlood) . trophyDefId)

        it "does not award anything for wrong answer" $
            checkAfterSubmit False (TrophyState 0 10)
                `shouldBe` []

        it "awards HatTrick for 3 correct streak" $
            checkAfterSubmit True (TrophyState 3 10)
                `shouldSatisfy` any ((== HatTrick) . trophyDefId)

        it "does not award HatTrick for 2 streak" $
            checkAfterSubmit True (TrophyState 2 10)
                `shouldSatisfy` all ((/= HatTrick) . trophyDefId)

        it "awards OnFire for 5 correct streak" $
            checkAfterSubmit True (TrophyState 5 10)
                `shouldSatisfy` any ((== OnFire) . trophyDefId)

        it "does not award OnFire for 4 streak" $
            checkAfterSubmit True (TrophyState 4 10)
                `shouldSatisfy` all ((/= OnFire) . trophyDefId)

        it "awards SpeedDemon for answer in under 5 seconds" $
            checkAfterSubmit True (TrophyState 1 4)
                `shouldSatisfy` any ((== SpeedDemon) . trophyDefId)

        it "does not award SpeedDemon for 5+ seconds" $
            checkAfterSubmit True (TrophyState 1 5)
                `shouldSatisfy` all ((/= SpeedDemon) . trophyDefId)

        it "awards multiple trophies at once" $ do
            let trophies = checkAfterSubmit True (TrophyState 5 3)
                ids = map trophyDefId trophies
            ids `shouldSatisfy` elem FirstBlood
            ids `shouldSatisfy` elem HatTrick
            ids `shouldSatisfy` elem OnFire
            ids `shouldSatisfy` elem SpeedDemon

    describe "checkAtFinish" $ do
        it "awards FlawlessVictory for perfect score" $
            checkAtFinish 10 10 Set.empty
                `shouldSatisfy` any ((== FlawlessVictory) . trophyDefId)

        it "does not award FlawlessVictory for imperfect score" $
            checkAtFinish 9 10 Set.empty
                `shouldSatisfy` all ((/= FlawlessVictory) . trophyDefId)

        it "awards ScholarSupreme for 90%+ with 10+ questions" $
            checkAtFinish 9 10 Set.empty
                `shouldSatisfy` any ((== ScholarSupreme) . trophyDefId)

        it "does not award ScholarSupreme for < 10 questions" $
            checkAtFinish 9 9 Set.empty
                `shouldSatisfy` all ((/= ScholarSupreme) . trophyDefId)

        it "does not award ScholarSupreme for < 90%" $
            checkAtFinish 8 10 Set.empty
                `shouldSatisfy` all ((/= ScholarSupreme) . trophyDefId)

        it "awards Marathoner for 20+ questions" $
            checkAtFinish 10 20 Set.empty
                `shouldSatisfy` any ((== Marathoner) . trophyDefId)

        it "does not award Marathoner for < 20 questions" $
            checkAtFinish 10 19 Set.empty
                `shouldSatisfy` all ((/= Marathoner) . trophyDefId)

        it "does not award FlawlessVictory for 0 questions" $
            checkAtFinish 0 0 Set.empty
                `shouldSatisfy` all ((/= FlawlessVictory) . trophyDefId)

        it "does not re-award already earned trophies" $
            checkAtFinish 10 10 (Set.singleton FlawlessVictory)
                `shouldSatisfy` all ((/= FlawlessVictory) . trophyDefId)

    describe "persistence" $ do
        it "returns empty set when no file exists" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                earned <- loadEarnedTrophies "/test/config.json"
                earned `shouldBe` Set.empty

        it "roundtrips earned trophies through save/load" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                let trophies = Set.fromList [FirstBlood, HatTrick]
                saveEarnedTrophies "/test/config.json" trophies
                loaded <- loadEarnedTrophies "/test/config.json"
                loaded `shouldBe` trophies

        it "uses separate files for different config paths" $ do
            withSystemTempDirectory "cert-prep-test" $ \tmpDir -> do
                setEnv "XDG_CONFIG_HOME" tmpDir
                let trophiesA = Set.fromList [FirstBlood]
                    trophiesB = Set.fromList [HatTrick, OnFire]
                saveEarnedTrophies "/config/a.json" trophiesA
                saveEarnedTrophies "/config/b.json" trophiesB
                loadedA <- loadEarnedTrophies "/config/a.json"
                loadedB <- loadEarnedTrophies "/config/b.json"
                loadedA `shouldBe` trophiesA
                loadedB `shouldBe` trophiesB
