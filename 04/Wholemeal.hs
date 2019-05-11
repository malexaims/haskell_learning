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
