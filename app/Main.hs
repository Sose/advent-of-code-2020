module Main where

import           Lib
import qualified Day01

main :: IO ()
main = do
    putStrLn "Day 01"
    d01 <- readInts "01"
    print $ Day01.part1 d01
    print $ Day01.part2 d01
