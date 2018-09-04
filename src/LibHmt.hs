{-# LANGUAGE OverloadedStrings #-}

module LibHmt where


import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Text as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"


test = "asdfasdf asdf asdfa fasdfadfadfadfasdf asdf asdf asdf asdfasdf asdf adsf asdf afd afd afd adf asdfasdfdfasdf dfasdfasdf asdf adsf"

x = "\ESC[31mHello World\ESC[0m"

data Symbol = Symbol String deriving (Show, Eq)
type Group = [Symbol]
type Line = [Group]

-- Consume the first Symbol within the String.
consumeSymbol :: String -> (Symbol, String)
consumeSymbol (x:xs) = (Symbol [x], xs)

-- Split a String into Symbols.
toSymbols :: String -> [Symbol]
toSymbols = chop consumeSymbol

-- Convert a Symbol back to a String.
symbolToString :: Symbol -> String
symbolToString (Symbol x) = x

-- Split a list of symbols into Groups.
toGroups :: [Symbol] -> [Group]
toGroups ss = split (noBlanks . dropFinalBlank $ oneOf [Symbol " ", Symbol "\n"]) ss
    where
        noBlanks = dropInitBlank . dropInnerBlanks . dropFinalBlank

-- Convert a Group back to a String.
groupToString :: Group -> String
groupToString g = concatMap symbolToString g

-- Consume the first couple of Groups to form a Line and return it as well as
-- the remaining Groups.
consumeLine :: Int -> [Group] -> (Line, [Group])
consumeLine w gs = (l ++ l', rest')
    where
        -- Make a list containing the cumulative lengths of the to-be-consumed
        -- Line when adding each Group one by one.
        cumSum = scanl1 (\acc l -> acc + l) (map length gs)
        -- Get the index of the longest list of Groups that still fits a Line.
        i = length (takeWhile (<= w) cumSum)
        -- Get the index of the first Newline character (or infinity ...)
        j = fromMaybe (maxBound :: Int) (elemIndex [Symbol "\n"] gs)
        -- Either split the list at the first Newline or at the longest list of
        -- Groups that still fit. In either case, make sure we consume at least
        -- one Group.
        k = max 1 (min i j)
        -- Do the actual split of the input Groups. If the remainder contains
        -- spaces, they are shifted "leftwards" from the remainder to the Line.
        (l, rest) = splitAt k gs
        (l', rest') = span (==[Symbol " "]) rest

-- Split a list of Groups into Lines.
toLines :: Int -> [Group] -> [Line]
toLines w = chop (consumeLine w)

-- Convert a Line back to a String.
lineToString :: Line -> String
lineToString l = concatMap (groupToString) l

-- Remove trailing whitespaces and newline
lineToCleanString :: Line -> String
lineToCleanString l = clean $ lineToString l
    where
        -- clean "\n" = ""
        clean s = filter (/= '\n') $ T.unpack $ T.strip $ T.pack $ s

hmtWith :: Int -> String -> String
hmtWith w s = intercalate "\n" (map lineToCleanString lines)
    where 
        lines = (toLines w . toGroups . toSymbols) s

hmt = hmtWith 79
