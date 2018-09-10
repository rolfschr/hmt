module Main where

import Data.List
import System.Directory
import System.Environment
import System.Exit
import Control.Monad.Extra

import LibHmt

-- TODO add version + git hash to ouptut , see https://haskell-lang.org/library/optparse-applicative

-- Split the input Strings into a list of (existing) files and the remainder.
extractFiles :: [FilePath] -> IO ([FilePath], [FilePath])
extractFiles args = partitionM doesFileExist args

-- Read input to be fed to hmt. Either from files provided on the command line
-- or from stdin.
getInput :: [String] -> IO String
getInput args = do
    (files, otherArgs) <- extractFiles args
    if (length files) > 0
        then concatMapM readFile files
        else getContents

main :: IO ()
main = do
    args <- getArgs
    (files, otherArgs) <- extractFiles args
    stdin <- getInput args
    (exitcode, stdout, stderr) <- hmtWith "fmt" otherArgs stdin
    if (exitcode == ExitSuccess)
        then putStr stdout
        else putStr $ (show exitcode) ++ ": " ++ stderr
    exitWith exitcode
