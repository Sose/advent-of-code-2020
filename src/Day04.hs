module Day04 where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read

type Field = (String, String)

type Passport = [Field]

-- required fields for p1... "cid" is not required
reqFields :: [String]
reqFields = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parseField :: Parser Field
parseField = do
  key <- manyTill (satisfy isAlpha) (char ':')
  value <- many (satisfy (not . isSpace))
  return (key, value)

parsePassport :: Parser Passport
parsePassport = many (parseField <* satisfy isSpace)

parseInput :: Parser [Passport]
parseInput = parsePassport `sepBy` char '\n'

-- does a single passport have only all the required fields
hasReqFields :: Passport -> Bool
hasReqFields p = reqFields == sort (delete "cid" (map fst p))

part1 :: [Passport] -> Int
part1 = length . filter hasReqFields

-- part 2
myBetween :: Ord a => a -> a -> a -> Bool
myBetween lo hi x = lo <= x && x <= hi

isHex :: Char -> Bool
isHex c = isDigit c || myBetween 'a' 'f' c

validByr, validIyr, validEyr :: Int -> Bool
validByr = myBetween 1920 2002
validIyr = myBetween 2010 2020
validEyr = myBetween 2020 2030

validHgt :: String -> Bool
validHgt s = case hgtUnit of
  "cm" -> myBetween 150 193 hgt
  "in" -> myBetween 59 76 hgt
  _ -> False
  where
    (hgtStr, hgtUnit) = span isDigit s
    hgt = read hgtStr :: Int

validHcl :: String -> Bool
validHcl ('#' : cs) = length cs == 6 && all isHex cs
validHcl _ = False

validEcl :: String -> Bool
validEcl ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: String -> Bool
validPid pid = length pid == 9 && all isDigit pid

isValid :: Passport -> Bool
isValid p = isJust $ do
  lookup "byr" p >>= readMaybe >>= guard . validByr
  lookup "iyr" p >>= readMaybe >>= guard . validIyr
  lookup "eyr" p >>= readMaybe >>= guard . validEyr
  lookup "hgt" p >>= guard . validHgt
  lookup "hcl" p >>= guard . validHcl
  lookup "ecl" p >>= guard . validEcl
  lookup "pid" p >>= guard . validPid

part2 :: [Passport] -> Int
part2 = length . filter isValid

main :: IO ()
main = do
  putStrLn "Day 04"
  passports <- parsedInput "04" parseInput
  print (part1 passports)
  print (part2 passports)
