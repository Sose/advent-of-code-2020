module Day05 where

import           Lib
import           Data.List                      ( sort )

choose :: (Int, Int) -> Char -> (Int, Int)
choose (lo, hi) c | c == 'F' || c == 'L' = (lo, (lo + hi) `div` 2)
                  | c == 'B' || c == 'R' = ((lo + hi) `div` 2 + 1, hi)
                  | otherwise = error "Error in choose (unknown character)"

seat :: String -> (Int, Int)
seat x = (row, col)
  where
    (row, _) = foldl choose (0, 127) (take 7 x)
    (col, _) = foldl choose (0, 7) (drop 7 x)

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

findHole :: [Int] -> Int
findHole (a : b : _) | a + 2 == b = a + 1
findHole (_ : xs)                 = findHole xs

main :: IO ()
main = do
    putStrLn "Day 05"
    codes <- readLines "05"
    let seatIds = seatId . seat <$> codes
    print $ maximum seatIds
    print $ (findHole . sort) seatIds
