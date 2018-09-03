module Main where

import Options.Applicative

import LibHmt
import Options

main :: IO ()
main = do
    options <- execParser opts
    let w = width options
    interact (hmtWith w)
