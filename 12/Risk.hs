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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

--Ex. 2
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
        skirm check (Battlefield a d)
              | check = Battlefield (a-1) d
              | otherwise = Battlefield a (d-1)
        dice n = replicateM n die
