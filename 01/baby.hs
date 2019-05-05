doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

head' :: [a] -> a
head' [] = error "No head on empty list"
head' (x:_) = x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = 1 + sum' xs

sum'' :: (Num b) => [b] -> b
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight!"
  | bmi <= 25.5  = "You're normal"
  | bmi <= 30.5 = "You're overweight"
  | otherwise   = "You are huge!"
  where bmi = weight / (height ^ 2)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sa = 2 * pi * r * h
      ta = pi * r ^2
  in sa + (2 * ta)

[let sq x = x * x in (sq 2, sq 4, sq 50)]
