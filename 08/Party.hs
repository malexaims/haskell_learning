{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = x}) (GL es fs) = GL (emp:es) (fs + x)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Semigroup GuestList where
  (GL es1 fs1) <> (GL es2 fs2)   = GL (es1++es2) (fs1+fs2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
              | gl1 < gl2 = gl2
              | gl1 > gl2 = gl1

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

excercise2 = print $ treeFold (\x xs -> glCons x $ mconcat xs) testCompany
