{-# LANGUAGE LambdaCase #-}
module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Char

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Monad Parser where
    p1 >>= fp2 = Parser $ \cs -> do
        (a, cs') <- runParser p1 cs
        runParser (fp2 a) cs'

    return x = Parser (\cs -> [(x, cs)])

instance MonadFail Parser where
    fail _ = Parser (const [])

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftA

instance Alternative Parser where
    empty = Parser $ const []
    p1 <|> p2 = Parser $ \cs -> case runParser (p1 `choose` p2) cs of
        []    -> []
        x : _ -> [x]

choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = Parser (\cs -> runParser p1 cs ++ runParser p2 cs)

item :: Parser Char
item = Parser $ \case
    ""       -> []
    (c : cs) -> [(c, cs)]

eof :: Parser ()
eof = Parser $ \case
    [] -> [((), "")]
    _  -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy check = Parser $ \case
    ""       -> []
    (c : cs) -> [ (c, cs) | check c ]

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = mapM char

alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

int :: Parser Int
int = do
    n <- string "-" <|> return []
    s <- some digit
    return (read (n ++ s) :: Int)

posInt :: Parser Int
posInt = do
    s <- some digit
    return (read s :: Int)
