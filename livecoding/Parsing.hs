{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Data.Char

-- functional languages are very good for parsing
-- Monadic Parser with do notation
-- for example for expressions like 2*3+4 as Tree

newtype Parser a = P (String -> [(a, String)]) -- state monad

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])
                    
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                           [] -> []
                           [(a, rest)] -> [(f a, rest)])

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\inp -> [(a, inp)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) pa px = P (\inp -> case parse pa inp of
                              [] -> []
                              [(f, rest)] -> parse (fmap f px) rest)

-- parse (pure 1) "abc"

three :: Parser (Char, Char, Char)
three = pure g <*> item <*> item <*> item
    where
        g :: Char -> Char -> Char -> (Char, Char, Char)
        g x y z = (x,y,z)

-- parse three "abcdef"
-- parse three "ab"

instance Monad Parser where
    return :: a -> Parser a
    return = pure -- macht das gleiche wie beim Applicative

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) pa cont = P (\inp -> case parse pa inp of
                                 [] -> []
                                 [(a, rest)] -> parse (cont a) rest)

threeMonadic :: Parser (Char, Char, Char)
threeMonadic = do
    x <- item
    y <- item
    z <- item
    return (x,y,z)

-- parse threeMonadic "abcdef"
-- parse threeMonadic "ab"

instance Alternative Parser where
    empty :: Parser a
    empty = P (\_ -> [])

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) pa1 pa2 = P (\inp -> case parse pa1 inp of
                                 [] -> parse pa2 inp
                                 ret -> ret)

-- parse emtpy "abc"
-- parse (item <|> return 'd') "abc"
-- parse (item <|> return 'd') ""

parseIf :: (Char -> Bool) -> Parser Char
parseIf p = do
    c <- item
    if p c 
        then return c
        else empty

-- parse (parseIf isDigit) "abc"
-- parse (parseIf isDigit) "1bc"

char :: Char -> Parser Char
char c = parseIf (==c)

string :: String -> Parser String
string [] = return []
string str@(c:cs) = do
    char c
    string cs
    return str


