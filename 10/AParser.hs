{- CIS 194 HW 10
   due Monday, 1 April
-}

{-# LANGUAGE OverloadedStrings #-}

module AParser where

import           Control.Applicative

import           Data.Char
import           qualified Data.Text as T

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
--Ex. 1
instance Functor Parser where
  fmap f1 (Parser f2) = Parser $ fmap (first f1) . f2

--Applies function f to head of tuple
first :: (a -> b) -> (a,c) -> (b,c)
first f (x, y) = (f x, y)

--Ex. 2
instance Applicative Parser where
  pure a = Parser (\xs -> Just (a, xs))
  p1 <*> p2 = Parser f3
        where f3 str = case runParser p1 str of --First run p1
                        Nothing -> Nothing --If either p1 or p2 fails, the whole thing fails
                        Just (y, ys) -> case runParser p2 ys of --Pass remaining inputs to p2
                          Nothing -> Nothing  --If either p1 or p2 fails, the whole thing fails
                          Just (z, zs) -> Just (y z, zs) --Return result of applying the function to the value

--Ex 3.
abParser :: Parser (Char, Char)
-- abParser = Parser f
--   where f xs
--           | T.isInfixOf "ab" $ T.pack xs = Just (('a', 'b'), rest)
--           | otherwise = Nothing
--           where rest = filter (\x -> (x /= 'a') && (x /= 'b')) xs
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
-- abParser_ = Parser f
--   where f xs
--           | T.isInfixOf "ab" $ T.pack xs = Just ((), rest)
--           | otherwise = Nothing
--           where rest = filter (\x -> (x /= 'a') && (x /= 'b')) xs
-- abParser_ = const () <$> ((,) <$> char 'a' <*> char 'b')
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  p1 <|> p2 = Parser p
              where p str = case runParser p1 str of
                              Just (y, ys) -> Just (y, ys)
                              Nothing -> case runParser p2 str of
                                Just (z, zs) -> Just (z, zs)
                                Nothing -> Nothing

upper :: Parser Char
upper = Parser f
  where f (x:xs)
          | isUpper x = Just (x, xs)
          | otherwise = Nothing

upper_ :: Parser ()
upper_ = const () <$> upper

intOrUppercase :: Parser ()
intOrUppercase = (const ()) <$> posInt <|> upper_
