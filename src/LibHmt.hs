{-# LANGUAGE OverloadedStrings #-}

module LibHmt where

import Data.Char
import Data.List
import Data.List.Split
import System.Process
import System.Exit

-- A "Symbol" represent either a single Character, a whitespace or an escape
-- sequence.
data Symbol = Character Char | Whitespace Char | EscSeq String deriving (Show, Eq)

-- Consume the first Symbol within the String.
consumeSymbol :: String -> (Symbol, String)
consumeSymbol a@('\ESC':xs) = (EscSeq e, xs') 
    where
        i = case (elemIndex 'm' xs) of
            Just i -> (i + 2) -- dont forget the '\ESC' at front and 'm' at back
            Nothing -> (maxBound :: Int) -- no closing 'm' -> take everything
        (e, xs') = splitAt i a
consumeSymbol (x:xs)
    | isSpace x = (Whitespace x, xs)
    | otherwise = (Character x, xs)

-- Split a String into Symbols.
toSymbols :: String -> [Symbol]
toSymbols = chop consumeSymbol

-- Test whether the Symbol is an EsqSeq.
isEscSeq :: Symbol -> Bool
isEscSeq (EscSeq _) = True
isEscSeq _ = False

-- Test whether the Symbol is a Whitespace.
isWhitespace :: Symbol -> Bool
isWhitespace (Whitespace _) = True
isWhitespace _ = False

-- Convert a Symbol back to a String.
symbolToString :: Symbol -> String
symbolToString (Character x) = [x]
symbolToString (Whitespace x) = [x]
symbolToString (EscSeq x) = x

symbolsToString :: [Symbol] -> String
symbolsToString = concatMap symbolToString

-- Zip two Symbol lists. Identical elements are simply taken. However, the
-- first list is expected to contain additional Whitespaces whereas the second
-- list is expected to contain additional EsqSeqs. Those Symbols are
-- additionally inserted into the result.
zipSymbols :: [Symbol] -> [Symbol] -> [Symbol]
zipSymbols ssFmt ssOrig = reverse (zipSymbols' ssFmt ssOrig [])
zipSymbols' [] ssOrig ss = (reverse ssOrig) ++ ss
zipSymbols' ssFmt [] ss = (reverse ssFmt) ++ ss
zipSymbols' (f:ssFmt) (o:ssOrig) ss
    | f == o = zipSymbols' ssFmt ssOrig (f:ss) -- no change
    | isEscSeq o = zipSymbols' (f:ssFmt) ssOrig (o:ss) -- add missing esqseq
    | isWhitespace f = zipSymbols' ssFmt (o:ssOrig) (f:ss) -- newly added whitespace
    | isWhitespace o = zipSymbols' (f:ssFmt) (ssOrig) ss -- removed whitespace
--    | otherwise = should not be possible ...


-- Extract EscSeqs from input, run `fmt` on remainder and insert EscSeqs again.
hmtWith :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
hmtWith exec args stdin = do
    let ss = toSymbols stdin
    let ssWithoutEsqSeqs = filter (not . isEscSeq) ss
    --let (eiss, ss) = extractEscSeqs $ toSymbols stdin
    (exitcode, stdout, stderr) <- fmtWith exec args (symbolsToString ssWithoutEsqSeqs)
    -- Insert EscSeqs only after successful `fmt`. Not sure this is the best
    -- way to do it ...
    let stdout' = if (exitcode == ExitSuccess)
                  then symbolsToString $ zipSymbols (toSymbols stdout) ss
                  else stdout
    return (exitcode, stdout', stderr)

fmtWith :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
fmtWith exec args stdin = readProcessWithExitCode exec args stdin
