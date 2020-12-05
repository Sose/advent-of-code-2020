module Day02 where

import           Lib
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Char                      ( isAlpha )
import           Text.Megaparsec.Char.Lexer     ( decimal )

data Policy = Policy Int Int Char
    deriving (Eq, Show)

data DBEntry = DBEntry Policy String
    deriving (Eq, Show)

policy :: Parser Policy
policy =
    Policy <$> decimal <* char '-' <*> decimal <* space <*> satisfy isAlpha

password :: Parser String
password = some (satisfy isAlpha)

entryParser :: Parser DBEntry
entryParser = DBEntry <$> policy <* char ':' <* space <*> password

-- Count how many times a char occurs in a string
countChars :: Char -> String -> Int
countChars c s = length (filter (== c) s)

-- Validate a single DBEntry according to part 1
entryIsValidP1 :: DBEntry -> Bool
entryIsValidP1 (DBEntry (Policy minc maxc c) pass) =
    minc <= cOccurs && cOccurs <= maxc
    where cOccurs = countChars c pass

-- Part 2 rules
entryIsValidP2 :: DBEntry -> Bool
entryIsValidP2 (DBEntry (Policy p1 p2 c) pass) =
    (length pass > pos1 && length pass > pos2)
        && (  (pass !! pos1 == c && pass !! pos2 /= c)
           || (pass !! pos1 /= c && pass !! pos2 == c)
           )
  where
    pos1 = p1 - 1 -- positions start from 1 instead of 0
    pos2 = p2 - 1

doPart :: (DBEntry -> Bool) -> [DBEntry] -> Int
doPart partFn es = length (filter id (partFn <$> es))

--- Read input and calculate both p1 and p2 and print the results

main :: IO ()
main = do
    putStrLn "Day 02"
    entries <- parsedInputLines "02" entryParser
    let p1      = doPart entryIsValidP1 entries
    let p2      = doPart entryIsValidP2 entries
    print p1
    print p2
