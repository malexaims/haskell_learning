module JoinList where

import Data.Monoid
import Sized

--m is for tracking monodial annotations to the structure
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex 1
tag :: Monoid m => JoinList m a -> m
tag (Single m1 _ )    = m1
tag (Append m1 _ _ )  = m1
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append m1 j1 j2
    where m1 = (tag j1 <> tag j2)

--Ex 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i j = (!!?) (jlToList j) i

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:xs) 0 = Just x
(!!?) (x:xs) i = (!!?) xs (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ j1 j2) = l1 ++ l2
    where l1 = jlToList j1
          l2 = jlToList j2

dropJ :: (Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 (Single m a) = Single m a
dropJ _ (Single m a) = Empty
dropJ 0 j = j
dropJ i j@(Append m j1 j2)
                      | i < 0 = Empty
                      | i < sizeJ1 = (dropJ i j1) +++ j2
                      | i > 0 = dropJ (i - sizeJ1) j2
                      | otherwise = j
                        where sizeJ1 = length . jlToList $ j1

someJoinList =
  Append (Product 210)
    (Append (Product 30)
      (Single (Product 5) 'y')
      (Append (Product 6)
        (Single (Product 2) 'e')
        (Single (Product 3) 'a')))
        (Single (Product 7) 'h')
