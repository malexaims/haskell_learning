import qualified Data.Bits as B
import Numeric (showIntAtBase, readInt)
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
-- import qualified Data.Number as C

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 = map fib [0..]

fib2 :: Int -> Integer
fib2 n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

data Stream a = Cons a (Stream a) deriving (Eq, Ord)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed ((+) 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = Cons x
                                            $ interleaveStreams s2 s1


getLog2 :: (B.Bits a, Integral a, Integral b) => a -> b
getLog2 0 = 0
getLog2 n = floor $ logBase (fromIntegral 2)
            $ fromIntegral $ (B..&.) n (-n)

ruler :: Stream Integer
ruler = interleaveStreams (streamMap getLog2 nats) (streamRepeat 0)
