{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.Char
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Bag = (String, [(Int, String)])

ex = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."

exl = lines ex

pex = case sequence $ parse bag "ex" <$> exl of
  Left _ -> error "asd"
  Right x -> x

goal :: String
goal = "shiny gold"

bagName :: Parser String
bagName = some letterChar <> " " <> some letterChar <* " bag" <* optional "s"

bag :: Parser Bag
bag = do
  b <- bagName <* " contain "
  bs <- [] <$ "no other bags" <|> ((,) <$> decimal <* " " <*> bagName) `sepBy` ", "
  _ <- "."
  return (b, bs)

myCount :: Foldable t => (a -> Bool) -> t a -> Int
myCount p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

main :: IO ()
main =
  do
    inp <- parsedInputLines "07" bag
    let m = contents inp
    print $ m
    print $ myCount (Map.member goal) m
    print (sum (m Map.! goal))

contents :: [Bag] -> Map String (Map String Int)
contents rules = m
  where
    m = expand <$> Map.fromList rules

    expand inside =
      Map.unionsWith
        (+)
        [ fmap (n *) $
            Map.insertWith (+) sub 1 $
              m Map.! sub
          | (n, sub) <- inside
        ]
