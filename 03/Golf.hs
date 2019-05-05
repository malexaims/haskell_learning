{-# OPTIONS_GHC -Wall #-}
module Golf where


{-|
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
|-}

skips :: [a] -> [[a]]
skips xs  =
    let selection = [[i, (2*i)+1 .. xLen] | i <- [0,1 .. xLen]]
        xLen = length xs - 1
    in map (map (xs !!)) selection

-- lmFilter :: [a]-> Bool
-- lmFilter [] = False
-- lmFilter [x:y:z]
--     | x < y && y > z = True
--     | otherwise      = False
-- lmFilter _ = False

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = [(xs !! i) | i <- [1,2 .. penult],
                                      let a = xs !! (i-1),
                                      let c = xs !! (i+1),
                                      a < (xs !! i),
                                      c < (xs !! i)]
        where penult = length xs - 2
