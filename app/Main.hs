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
    (exitcode, stdout, stderr) <- getContents >>= hmtWith ex ar
    if (exitcode == ExitSuccess)
        then putStr stdout
        else putStr $ (show exitcode) ++ ": " ++ stderr
    exitWith exitcode
