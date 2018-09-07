module Main where

import LibHmt
import System.Exit
import System.Environment

-- TODO add version + git hash to ouptut , see https://haskell-lang.org/library/optparse-applicative

main :: IO ()
main = do
    args <- getArgs
    (exitcode, stdout, stderr) <- getContents >>= hmtWith "fmt" args
    if (exitcode == ExitSuccess)
        then putStr stdout
        else putStr $ (show exitcode) ++ ": " ++ stderr
    exitWith exitcode
