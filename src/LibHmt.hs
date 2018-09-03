{-# LANGUAGE OverloadedStrings #-}

module LibHmt where


import Data.Char
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
toGroups ss = split (dropInnerBlanks . dropFinalBlank $ oneOf [Symbol " ", Symbol "\n"]) ss

-- Convert a Group back to a String.
groupToString :: Group -> String
groupToString g = concatMap symbolToString g

-- Consume the first couple of Groups to form a Line and return it as well as
-- the remaining Groups.
consumeLine :: Int -> [Group] -> (Line, [Group])
--consumeLine w [g] = ([g], [])
--consumeLine w gs = if i == 0 then ([head gs], tail gs) else splitAt i gs
consumeLine w gs = splitAt i gs
    where
        -- Make a list containing the cumulative length of the to-be-consumed
        -- Line (i.e. when adding each Group one by one). Use `tail` to remove
        -- the [0] at the head.
        cumSum = scanl1 (\acc l -> acc + l) (map length gs)
        -- Get the index of the longest list of Groups that still fits a Line.
        -- Take at least one group.
        i = max 1 (length (takeWhile (<= w) cumSum))

-- Split a list of Groups into Lines.
toLines :: Int -> [Group] -> [Line]
toLines w = chop (consumeLine w)

-- Convert a Line back to a String removing the leading whitespace(s).
lineToString :: Line -> String
lineToString l = T.unpack $ T.strip $ T.pack $ concatMap (groupToString) l

hmtWith :: Int -> String -> String
hmtWith w s = intercalate "\n" (map lineToString lines)
    where 
        lines = (toLines w . toGroups . toSymbols) s

hmt = hmtWith 79
