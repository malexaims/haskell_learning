-- {-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT as E
import Parser as P
import qualified Data.Maybe as Y
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = E.Lit x
  add x y = E.Add x y
  mul x y = E.Mul x y

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
      | (x >= 0)  = True
      | otherwise = False
  add = (&&)
  mul = (||)

newtype MinMax = MinMax Integer
  deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = min
  mul = max

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show, Ord)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x+y
  mul (Mod7 x) (Mod7 y) = lit $ x*y

class HasVars a where
  var :: String -> a

data VarExprT = Vlit Integer
         | Vadd VarExprT VarExprT
         | Vmul VarExprT VarExprT
         | Vvar Integer
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = Vlit x
  add x y = Vadd x y
  mul x y = Vmul x y

instance HasVars VarExprT where
  var x = Vvar (read x::Integer)

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\_ -> Just x)
  add f g = \m -> case ((f m == Nothing) || (g m == Nothing)) of
                    True -> Nothing
                    False -> Just (Y.fromJust (f m) +
                                   Y.fromJust (g m))
  mul f g = \m -> case ((f m == Nothing) || (g m == Nothing)) of
                    True -> Nothing
                    False -> Just (Y.fromJust (f m) *
                                   Y.fromJust (g m))

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr expr = case (P.parseExp E.Lit E.Add E.Mul expr) of
                  Just s -> Just $ eval s
                  Nothing -> Nothing

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
