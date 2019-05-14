{-# OPTIONS_GHC -Wall -Werror #-}

import ExprT as E
import Parser as P

eval :: ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr expr = case (P.parseExp E.Lit E.Add E.Mul expr) of
                  Just s -> Just $ eval s
                  Nothing -> Nothing
