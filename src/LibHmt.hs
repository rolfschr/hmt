{-# LANGUAGE OverloadedStrings #-}

module LibHmt where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.Process
import System.Exit

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

-- Assign each Symbol an index:
-- - Whitespaces get index 0.
-- - Characters get a uniqe index.
-- - EscSeqs get the _same_ index as their next Character.
indexSymbols :: [Symbol] -> [(Int, Symbol)]
indexSymbols ss = snd $ foldr f (0, []) ss
    where
        f s@(Whitespace _) (lastIndex, iss) = (lastIndex, (0, s):iss)
        f s@(EscSeq _) (lastIndex, iss) = (lastIndex, (lastIndex, s):iss)
        f s (lastIndex, iss) = (lastIndex + 1, (lastIndex + 1, s):iss)

-- Extract EscSeqs with their original index from the Symbols and return them
-- together with the list of remaining Symbols.
extractEscSeqs :: [Symbol] -> ([(Int, Symbol)], [Symbol])
extractEscSeqs ss = (ess, map snd noness)
    where
        iss = indexSymbols ss
        (ess, noness) = partition (\(_, s) -> isEscSeq s) iss
        isEscSeq (EscSeq _) = True
        isEscSeq _ = False

-- Insert EscSeqs at their original position into the list of Symbols.
insertEscSeqs :: [(Int, Symbol)] -> [Symbol] -> [Symbol]
insertEscSeqs eiss ss = snd $ foldr f (reverse eiss, []) iss
    -- use (revserse eiss) because we consume the Symbols from back to front;
    -- i.e. we need to consume the last EscSeq first
    where
        iss = indexSymbols ss
        f (_, s) ([], ss) = ([], s:ss) -- no escseqs left, simply consume rest
        f (sIndex, s) ((eIndex, e):eiss, ss)
            | sIndex == eIndex = (eiss, e:s:ss) -- insert escseq before current symbol
            | otherwise        = ((eIndex, e):eiss, s:ss) -- wait

-- Convert a Symbol back to a String.
symbolToString :: Symbol -> String
symbolToString (Character x) = [x]
symbolToString (Whitespace x) = [x]
symbolToString (EscSeq x) = x

symbolsToString :: [Symbol] -> String
symbolsToString = concatMap symbolToString

-- Extract EscSeqs from input, run `fmt` on remainder and insert EscSeqs again.
hmtWith :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
hmtWith exec args stdin = do
    let (eiss, ss) = extractEscSeqs $ toSymbols stdin
    (exitcode, stdout, stderr) <- fmtWith exec args (symbolsToString ss)
    -- Insert EscSeqs only after successful `fmt`. Not sure this is the best
    -- way to do it ...
    let stdout' = if (exitcode == ExitSuccess)
                  then symbolsToString $ insertEscSeqs eiss (toSymbols stdout)
                  else stdout
    return (exitcode, stdout', stderr)

fmtWith :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
fmtWith exec args stdin = readProcessWithExitCode exec args stdin
