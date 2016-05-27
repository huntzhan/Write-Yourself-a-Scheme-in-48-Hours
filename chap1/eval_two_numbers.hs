module Main where

import System.Environment

-- v1.
-- main = do
--         args <- getArgs
--         let a1 = read args !! 0 :: Int
--             a2 = read args !! 1 :: Int
--         in putStrLn . show $ a1 + a2
--
-- Error: Parse error: Last statement in a do-block must be an expression
-- Found:
--         let a1 = read args !! 0 :: Int
--             a2 = read args !! 1 :: Int
--   >     in putStrLn . show $ a1 + a2
--
-- http://stackoverflow.com/questions/8274650/in-haskell-when-do-we-use-in-with-let
-- Short answer: Use let without in in the body of a do-block,
-- and in the part after the | in a list comprehension.
-- Anywhere else, use let ... in ....


-- v2.
-- main = do
--         args <- getArgs
--         let a1 = read args !! 0 :: Int
--         let a2 = read args !! 1 :: Int
--         putStrLn . show $ a1 + a2
-- 
-- Found:
--   read args !! 0
-- Why not:
--   head (read args)
-- 
-- eval_two_numbers.hs:28:9: Warning: Use print
-- Found:
--   putStrLn . show
-- Why not:
--   print


main = do
        args <- getArgs
        -- precedence of "::" is pretty low.
        let [a1, a2] = take 2 . map read $ args :: [Int]
        -- "print a1 + a2" <=> "(print a1) + a2"
        print (a1 + a2)

