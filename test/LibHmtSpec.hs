module LibHmtSpec (spec) where

import Test.Hspec

import LibHmt

toIndexedSymbolsFromString :: String -> [(Int, Symbol)]
toIndexedSymbolsFromString s = (indexSymbols . toSymbols) s

extractInsert :: [Symbol] -> [Symbol]
extractInsert = (uncurry insertEscSeqs) . extractEscSeqs

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
    describe "indexSymbols" $ do
        it "correct indexing (1)" $ do
            let s = "98765 4321"
            let iss = toIndexedSymbolsFromString s
            head iss `shouldBe` (9, Character '9')
            last iss `shouldBe` (1, Character '1')
            iss !! 5 `shouldBe` (0, Whitespace ' ')
    describe "(extract|insert)EscSeqs" $ do
        it "correct extract/insert (1)" $ do
            let ss = toSymbols "ABC\ESC[31mDEF"
            snd (extractEscSeqs ss) `shouldBe` toSymbols "ABCDEF"
            extractInsert ss `shouldBe` ss
        it "correct extract/insert (2)" $ do
            let ss = toSymbols "ABC \ESC[31mDEF"
            extractInsert ss `shouldBe` ss
        it "correct extract/insert (2)" $ do
            let ss = toSymbols "ABC \ESC[31m DEF"
            -- Note that the EscSeq has shifted rightwards.
            extractInsert ss `shouldBe` toSymbols "ABC  \ESC[31mDEF"
