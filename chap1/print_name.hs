module Main where

import System.Environment

main = do
        putStrLn "please enter your name:"
        name <- getLine
        putStrLn name
