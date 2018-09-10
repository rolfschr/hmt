module LibHmtSpec (spec) where

import Test.Hspec

import LibHmt

spec :: Spec
spec = do
    describe "consumeSymbol" $ do
        it "correct consuming (1)" $ do
            let s = "A"
            consumeSymbol s `shouldBe` (Character 'A', [])
            let s = "AB"
            consumeSymbol s `shouldBe` (Character 'A', "B")
        it "correct consuming (2)" $ do
            let s = "A\ESC[31m"
            fst (consumeSymbol s) `shouldBe` Character 'A'
            let s = "\ESC[31mA"
            consumeSymbol s `shouldBe` (EscSeq "\ESC[31m", "A")
            let s = "\ESC[0mA"
            consumeSymbol s `shouldBe` (EscSeq "\ESC[0m", "A")
        it "correct consuming (3)" $ do
            let s = " A"
            fst (consumeSymbol s) `shouldBe` Whitespace ' '
            let s = "\nA"
            fst (consumeSymbol s) `shouldBe` Whitespace '\n'
            let s = "\tA"
            fst (consumeSymbol s) `shouldBe` Whitespace '\t'
        it "correct consuming (4)" $ do
            let s = "\ESC[31never closed" 
            fst (consumeSymbol s) `shouldBe` EscSeq "\ESC[31never closed"
    describe "zipSymbols" $ do
        it "correct zipping (1)" $ do
            let ssFmt = toSymbols "ABC DEFG"
            zipSymbols ssFmt ssFmt `shouldBe` ssFmt
        it "correct zipping (2)" $ do
            let ssFmt = toSymbols "ABC DEFG "
            let ssOrig = toSymbols "ABC DEFG \ESC[31m\ESC[0m"
            zipSymbols ssFmt ssOrig `shouldBe` ssOrig
        it "correct zipping (3)" $ do
            let ssFmt = toSymbols "  ABC DEFG "
            let ssOrig = toSymbols "ABC DEFG "
            zipSymbols ssFmt ssOrig `shouldBe` ssFmt
        it "correct zipping (4)" $ do
            let ssFmt = toSymbols "ABC DEFG \n "
            let ssOrig = toSymbols "ABC DEFG "
            zipSymbols ssFmt ssOrig `shouldBe` ssFmt
        it "correct zipping (5)" $ do
            let ssFmt = toSymbols "ABC DEFG \n "
            let ssOrig = toSymbols "ABC \ESC[31mDEFG "
            let ss3 = toSymbols "ABC \ESC[31mDEFG \n "
            zipSymbols ssFmt ssOrig `shouldBe` ss3
