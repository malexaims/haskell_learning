{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if even n
                         then n `div` 2
                         else 3*n+1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ treeL nodeName treeR)
    | hL < hR   = Node (hR+1) (treeInsert x treeL) nodeName treeR
    | otherwise = Node (hL+1) treeL nodeName (treeInsert x treeR)
            where hL = height treeL
                  hR = height treeR

height :: Tree a -> Integer
height Leaf = 0
height (Node x _ _ _) = x

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf
