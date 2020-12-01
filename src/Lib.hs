module Lib where

readInput :: String -> IO String
readInput day = readFile $ "input/" ++ day ++ ".in"

readLines :: String -> IO [String]
readLines day = lines <$> readInput day

readInts :: String -> IO [Int]
readInts day = readLines day >>= return . (read <$>)
