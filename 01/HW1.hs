{-# LANGUAGE ParallelListComp #-}
toDigits :: Integer -> [Integer]
toDigits n
    | n < 10 = [n]
    | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
    | even (length (x:y:zs)) = x*2:y:doubleEveryOther(zs)
    | otherwise              = x:y*2:doubleEveryOther(zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = sum (toDigits x)
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x =
    if rem == 0
    then True
    else False
    where digits = toDigits x
          doubled = doubleEveryOther digits
          summed = sumDigits doubled
          rem = summed `mod` 10

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _                      = []
hanoi 1 fromPeg toPeg _            = [(fromPeg, toPeg)]
hanoi height fromPeg toPeg withPeg =
    let redH = height - 1
    in hanoi redH fromPeg withPeg toPeg ++
       hanoi 1 fromPeg toPeg withPeg ++
       hanoi redH withPeg toPeg fromPeg

moves :: Integer -> Peg -> Peg -> Peg -> Int
moves height fromPeg toPeg withPeg =
    length moves
    where moves = hanoi height fromPeg toPeg withPeg
