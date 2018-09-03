module LibHmtSpec (spec) where

import Test.Hspec

import LibHmt

spec :: Spec
spec = do
  describe "lines" $ do
    it "removes trailing whitepsaces at the beginning of a line" $ do
        let s =  " A text with whitespace prepended."
        let r = hmt s
        r `shouldBe` (tail s)
