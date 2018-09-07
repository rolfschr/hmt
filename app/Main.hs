module Main where

import Options.Applicative

import LibHmt
import Options
import System.Exit

main :: IO ()
main = do
    options <- execParser opts
    let w = width options
    --interact (hmtWith w)
    --x <- fmt "adfaf" -- <$> getContents
    -- stdin <- getContents
    (a, b, c) <- getContents >>= hmt
    -- print (show (typeOf x))
    putStr b
