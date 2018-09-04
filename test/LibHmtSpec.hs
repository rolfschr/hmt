module LibHmtSpec (spec) where

import Test.Hspec

import LibHmt

---- Run consumeLine and return the resulting String.
--getFstLineAsString :: Int -> String -> String
--getFstLineAsString w s = lineToString $ fst $ consumeLine w gs
--    where
--        gs = (toGroups . toSymbols) s
--
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

  --describe "lineToString" $ do
    --it "removes trailing whitepsaces in " $ do
    --    let s =  " A text with trailing whitespace. "
    --    getFstLineAsString 100 s `shouldBe` (init $ tail s)

  describe "consumeLine" $ do
    it "checks correct Line split (1)" $ do
        let s =  "This is the first line."
        let gs = toGroupsFromString s
        let (l, _) = consumeLine 5 gs
        l `shouldBe` toGroupsFromString "This "
    it "checks correct Line split (2)" $ do
        let s =  "This is \nthe first line."
        let gs = toGroupsFromString s
        let (l, _) = consumeLine 100 gs
        l `shouldBe` toGroupsFromString "This is "
    it "word longer than line (1)" $ do
        let s =  "longword"
        let gs = toGroupsFromString s
        let (l, _) = consumeLine 3 gs
        l `shouldBe` gs
    it "word longer than line (2)" $ do
        let s =  "longword "
        let gs = toGroupsFromString s
        print gs
        let (l, _) = consumeLine 3 gs
        l `shouldBe` gs
 -- todo: consumelineS 5 "second \n \n" --> only 3 lines!!! (check back with fmt?)
