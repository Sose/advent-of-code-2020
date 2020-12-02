module Day02 where

import           Control.Applicative            ( some )

import           Lib
import qualified Parser                        as P

type MinOcc = Int
type MaxOcc = Int
data Policy = Policy MinOcc MaxOcc Char
    deriving (Eq, Show)

type Password = String
data DBEntry = DBEntry Policy Password
    deriving (Eq, Show)

parseEntry :: String -> Maybe DBEntry
parseEntry s = case P.runParser entryParser s of
    [(x, "")] -> Just x
    _         -> Nothing

entryParser :: P.Parser DBEntry
entryParser = do
    min  <- P.posInt
    _    <- P.char '-'
    max  <- P.posInt
    _    <- P.space
    c    <- P.alpha
    _    <- P.char ':'
    _    <- P.space
    pass <- some P.alpha
    return $ DBEntry (Policy min max c) pass

-- Count how many times a char occurs in a string
countChars :: Char -> String -> Int
countChars c s = length $ filter (== c) s

-- Validate a single DBEntry according to part 1
entryIsValidP1 :: DBEntry -> Bool
entryIsValidP1 (DBEntry (Policy minc maxc c) pass) =
    let cOccurs = countChars c pass in minc <= cOccurs && cOccurs <= maxc

-- Part 2 rules
entryIsValidP2 :: DBEntry -> Bool
entryIsValidP2 (DBEntry (Policy p1 p2 c) pass) =
    let pos1 = p1 - 1 -- positions start from 1 instead of 0
        pos2 = p2 - 1
    in  (length pass > pos1 && length pass > pos2)
            && (  (pass !! pos1 == c && pass !! pos2 /= c)
               || (pass !! pos1 /= c && pass !! pos2 == c)
               )

-- part1 takes a list of Strings like "1-3 a: abcde" and counts how many
-- entries are valid
part1 :: [String] -> Either String Int
part1 strs = case sequence $ parseEntry <$> strs of
    Nothing   -> Left "Error while parsing input"
    (Just es) -> Right $ length $ filter (== True) $ entryIsValidP1 <$> es

part2 :: [String] -> Either String Int
part2 strs = case sequence $ parseEntry <$> strs of
    Nothing   -> Left "Error while parsing input"
    (Just es) -> Right $ length $ filter (== True) $ entryIsValidP2 <$> es

--- Read input and calculate both p1 and p2 and print the results

run :: IO ()
run = do
    ls <- readLines "02"
    let p1 = part1 ls
    let p2 = part2 ls
    putStrLn "Day 02"
    print p1
    print p2
