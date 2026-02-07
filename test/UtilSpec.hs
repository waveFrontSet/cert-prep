module UtilSpec (spec) where

import Test.Hspec
import Util (formatTime)

spec :: Spec
spec = do
    describe "formatTime" $ do
        it "formats zero seconds" $
            formatTime 0 `shouldBe` "00:00"
        it "formats seconds under a minute" $
            formatTime 45 `shouldBe` "00:45"
        it "formats exactly one minute" $
            formatTime 60 `shouldBe` "01:00"
        it "formats minutes and seconds" $
            formatTime 125 `shouldBe` "02:05"
        it "formats large values" $
            formatTime 3661 `shouldBe` "61:01"
        it "pads single digits" $
            formatTime 9 `shouldBe` "00:09"
