module LibHmtSpec (spec) where

import Test.Hspec

import LibHmt

-- Run consumeLine and return the resulting String.
getFstLineAsString :: Int -> String -> String
getFstLineAsString w s = lineToString $ fst $ consumeLine w gs
    where
        gs = (toGroups . toSymbols) s

toLinesFromString :: Int -> String -> [Line]
toLinesFromString w s = toLines w gs
    where
        gs = (toGroups . toSymbols) s

toGroupsFromString s = (toGroups . toSymbols) s

spec :: Spec
spec = do
  describe "toGroups" $ do
    it "checks correct Group split (1)" $ do
        let gs = (toGroups . toSymbols) "This "
        length gs `shouldBe` 2
        last gs  `shouldBe` [Symbol " "]
    it "checks correct Group split (2)" $ do
        let gs = (toGroups . toSymbols) "1\n\n2"
        gs `shouldBe` [[Symbol "1"],[Symbol "\n"],[Symbol "\n"],[Symbol "2"]]

  describe "lineToString" $ do
    it "removes trailing whitepsaces ng of a line" $ do
        let s =  " A text with trailing whitespace. "
        getFstLineAsString 100 s `shouldBe` (init $ tail s)

  describe "consumeLine" $ do
    it "checks correct Line split (1)" $ do
        let s =  "This is the first line."
        let gs = toGroupsFromString s
        let (l, _) = consumeLine 5 gs
        l `shouldBe` toGroupsFromString "This "
    it "makes sure words longer than the max line width are correctly consumed" $ do
        let s =  "longword"
        let gs = toGroupsFromString s
        let (l, _) = consumeLine 3 gs
        l `shouldBe` gs
