module Day01 where

import Lib

part1 :: [Int] -> Int
part1 l = head [a * b | a <- l, b <- l, a /= b, a + b == 2020]

part2 :: [Int] -> Int
part2 l = head [a * b * c | a <- l, b <- l, c <- l, a + b + c == 2020]

main :: IO ()
main = do
  putStrLn "Day 01"
  inp <- readInts "01"
  print $ Day01.part1 inp
  print $ Day01.part2 inp
