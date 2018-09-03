module LibHmt where

import Data.Char
import Data.List
import Data.List.Split

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

-- Split a list of symbols into Groups
toGroups :: [Symbol] -> [Group]
toGroups ss = split (oneOf [Symbol " "]) ss

-- Convert a Group back to a String.
groupToString :: Group -> String
groupToString g = concatMap symbolToString g

-- Consume the first Line within a list of Groups.
consumeLine :: Int -> [Group] -> ([Group], [Group])
consumeLine w gs = splitAt i gs
    where
        -- Make a list containing the cumulative length when adding each Group
        -- one by one. Use `tail` to remove the [0] at the head.
        cumSum = scanl (\acc ss -> acc + length ss) 0 gs
        -- Get the index of the highest [Group] that still fits a Line.
        i = length (takeWhile (<= w) cumSum)

-- Split a list of Groups into Lines.
toLines :: Int -> [Group] -> [Line]
toLines w = chop (consumeLine w)

-- Convert a Line back to a String removing the leading whitespace(s).
lineToString :: Line -> String
lineToString l = dropWhile isSpace $ concatMap (groupToString) l

hmtWith :: Int -> String -> String
hmtWith w s = intercalate "\n" (map lineToString lines)
    where 
        lines = (toLines w . toGroups . toSymbols) s

hmt = hmtWith 79
