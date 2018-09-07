module Main where

import Options.Applicative

import LibHmt
import Options
import System.Exit

-- TODO add version + git hash to ouptut , see https://haskell-lang.org/library/optparse-applicative

main :: IO ()
main = do
    options <- execParser opts
    let ex = exec options
    let ar = args options
    --interact (hmtWith w)
    --x <- fmt "adfaf" -- <$> getContents
    (a, b, c) <- getContents >>= hmtWith ex ar
    -- print (show (typeOf x))
    putStr b
