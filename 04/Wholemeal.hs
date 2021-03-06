{-# OPTIONS_GHC -Wall #-}

module Wholemeal where


--EX 1. Implement these using "wholemeal style"

-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--     | even x = (x - 2) * fun1 xs
--     | otherwise = fun1 xs

-- fun2 :: Integer -> Integer
--     fun2 1 = 0
--     fun2 n | even n = n + fun2 (n ‘div‘ 2)
--     | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if even n
                         then n `div` 2
                         else 3*n+1)

--Ex 2. Generate a balanced binary tree from a list of values using foldr
-- foldTree "ABCDEFGHIJ" ==
-- Node 3
--  (Node 2
--    (Node 0 Leaf ’F’ Leaf)
--    ’I’
--    (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
-- ’J’
-- (Node 2
--  (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--  ’H’
--  (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

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

--Ex 3.1 Implement a function
-- xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.

convBool :: Bool -> Int
convBool x
    | x         = 1
    | otherwise = 0

xor :: [Bool] -> Bool
xor = odd
      . foldl (+) 0
      . map convBool

-- 3.2 Implement map as a fold. That is, complete the definition
-- map’ :: (a -> b) -> [a] -> [b]
-- map’ f = foldr ...
-- in such a way that map’ behaves identically to the standard map
-- function.

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x accum -> f x : accum) [] xs

--Ex 4. Implement the sieve of sundaran using function composition

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys ]

sieve :: [a] -> [(a, a)]
sieve xs = cartProd xs xs

sundaram :: (Num a) => (a, a) -> a
sundaram (a, b) = a + b + (2*a*b)

sieveSundaram :: Integer -> [Integer]
sieveSundaram m = filter (<m)
                  $ map (\x -> 2*x + 1 )
                  $ filter (\x -> x `notElem` (map sundaram $ sieve l)) l
    where n = m `div` 2
          l = [1,2..n]
          {--I thought the where statements were more readable--}
          -- sols = map sundaram $ sieve l
          -- notelemSols a = a `notElem` sols
