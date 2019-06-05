{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice n = replicateM n die
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

--Ex. 2
skirm :: Bool -> Battlefield -> Battlefield
skirm check (Battlefield a d)
      | check = Battlefield (a-1) d
      | otherwise = Battlefield a (d-1)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  aRolls <- dice at
  dRolls <- dice def
  let rolls = zipWith (<) aRolls' dRolls'
      aRolls' = take def . reverse $ sort aRolls
      dRolls' = reverse $ sort dRolls
  return $ foldr skirm bf rolls
  where at = min 3 ((attackers bf) - 1) :: Army
        def = min 2 (defenders bf) :: Army


--Ex. 3
victory :: Battlefield -> Bool
victory bf
    | defenders bf == 0 = True
    | attackers bf < 2  = True
    | otherwise = False

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
    | victory bf = return bf
    | otherwise  = battle bf >>= invade

--Ex 4.
winner :: Battlefield -> Int --1 if attackers win, else 0
winner bf
  | defenders bf == 0 = 1
  | attackers bf < 2  = 0
  | otherwise = error "Use only on finished battles"

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    sims <- (replicateM 1000 $ invade bf)
    let awins = fromIntegral $ sum $ map winner sims
    return $ awins / (fromIntegral 1000)

main :: IO ()
main = do
  putStrLn "Number of Attackers: "
  a <- getLine
  putStrLn "Number of Defenders: "
  d <- getLine
  let battlef = Battlefield (read a) (read d)
  print =<< (evalRandIO $ battle battlef)
  print =<< (evalRandIO $ invade battlef)
  print =<< (evalRandIO $ successProb battlef)
