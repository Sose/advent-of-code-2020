module Day06 (main)
where

import           Lib
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char

inputParser :: Parser [[String]]
inputParser = (some letterChar `endBy` char '\n') `sepBy` char '\n'

part1 :: [[String]] -> Int
part1 xs = sum (length . foldr1 union <$> xs)

part2 :: [[String]] -> Int
part2 xs = sum (length . foldr1 intersect <$> xs)

main :: IO ()
main = do
    putStrLn "Day 06"
    i <- parsedInput "06" inputParser
    print $ part1 i
    print $ part2 i
