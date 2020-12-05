module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Void
import Data.Functor ((<&>))

type Parser = Parsec Void String

readInput :: String -> IO String
readInput day = readFile $ "input/" ++ day ++ ".in"

readLines :: String -> IO [String]
readLines day = lines <$> readInput day

readInts :: String -> IO [Int]
readInts day = readLines day <&> (read <$>)

parsedInput :: String -> Parser a -> IO a
parsedInput day p = do
  input <- readInput day
  case parse p "input" input of
    Left e -> fail (errorBundlePretty e)
    Right a -> return a

parsedInputLines :: String -> Parser a -> IO [a]
parsedInputLines day p = do
  lines <- readLines day
  case sequence (parse p "input" <$> lines) of
    Left e -> fail (errorBundlePretty e)
    Right a -> return a

