module Day03 where

import           Lib

atCoord :: [String] -> Int -> Int -> Maybe Char
atCoord m x y | y >= length m = Nothing
              | otherwise     = Just $ row !! wrappedX
  where
    row      = m !! y
    wrappedX = x `mod` length row

slope :: [String] -> Int -> Int -> String
slope m dx dy = go m dx dy 0
  where
    go m dx dy iters = case atCoord m (dx * iters) (dy * iters) of
        Nothing -> []
        Just c  -> c : go m dx dy (iters + 1)

countTrees :: String -> Int
countTrees = length . filter (== '#')

part1 :: [String] -> Int
part1 m = countTrees (slope m 3 1)

part2 :: [String] -> Int
part2 m = product trees
  where
    p2Slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    slopes   = uncurry (slope m) <$> p2Slopes
    trees    = countTrees <$> slopes

main :: IO ()
main = do
    putStrLn "Day 03"
    m <- readLines "03"
    print $ part1 m
    print $ part2 m
