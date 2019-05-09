{-# OPTIONS_GHC -Wall #-}

module Golf where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char


{-
Skips uses a list of lists, called selection, to filter the input list xs.
Each sublist in selection contains the indicies of the output elements of
each step of the hopscotch algorithm:

"The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list."

Selection is created using a embedded list comprehension where each sublist is generated
based on the sublist's index in selection. The length of selection
is equal to the length of the input list xs per the problem statement. For instance,

index = 0
returns: [0, 2*0+1, 3*0+2, ..] = [0, 1, 2, 3 ..]
index = 1
returns: [1, 2*1+1, 3*1+2, ..] = [1, 3, 5 ..]
=> selection = [[0,1,2,3 ..], [1, 3, 5 ..], ..]

An embedded map is used to map xs !! onto each element of each selection sublist
as a partial function. Each of these partial functions is then applied using the
indicies from the selection sublists as the second argument. This map returns the
characters of the original list xs that are at the indicies of each sublist. For example,

skips "ABCD" = map (map "ABCD" !!) selection
=> skips "ABCD" = [[map "ABCD" !! [0, 1, 2, 3 ..], [map "ABCD" !! [1, 3, 5 ..], ..]
=> skips "ABCD" = ["ABCD", "BD", "C", "D"]
-}

skips :: [a] -> [[a]]
skips xs  =
    let selection = [[i, (2*i)+1 .. xLen] | i <- [0,1 .. xLen]]
        xLen = length xs - 1
    in map (map (xs !!)) selection

{-
localMaxima takes a list of Integers and returns a list of the elements
that have lower values to the left and right. For instance,

[1,2,3,4] = []
[1,2,1,2,1] = []

The function uses a list comprehension with guards that select elements
that are local maxima by comparing them against the index before and after
-}

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [(xs !! i) | i <- [1,2 .. penult],
                                    let a = xs !! (i-1),
                                    let c = xs !! (i+1),
                                    a < (xs !! i),
                                    c < (xs !! i)]
                                    where penult = length xs - 2

makeListofList :: (a,a) -> [a]
makeListofList (a, b) = [a, b]

--Solution does not work fully since the histogram is
-- horizontal instead of vertical. Not sure how to
occurences :: (Num a, Ord a) => [a] -> [[a]]
occurences xs = map (makeListofList)
                    (Map.toList (Map.fromListWith
                    (+) [(x, 1) | x <- (List.sort xs)]))

prntLine :: (Show a, Integral a) => [a] -> String
prntLine [num, occur] = (show num) ++ "|" ++
                        replicate (fromIntegral occur) '*'
prntLine _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map prntLine (occurs)) ++ " 0" ++
               (filter Char.isAlphaNum (unwords $
               map show [1,2..maxOccur]))
               ++ "\n"
                  where maxOccur = maximum (map (!! 1) occurs)
                        occurs = occurences xs
