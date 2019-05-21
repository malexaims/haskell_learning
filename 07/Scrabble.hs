{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Show, Eq, Num, Ord)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

ones = "AEIOURSTLN"
twos = "DG"
threes = "BCMP"
fours = "FHVWY"
fives = "K"
eights = "JX"
tens = "QZ"

score :: Char -> Score
score c
      | elem c' ones   = Score 1
      | elem c' twos   = Score 2
      | elem c' threes = Score 3
      | elem c' fours  = Score 4
      | elem c' fives  = Score 5
      | elem c' eights = Score 8
      | elem c' tens   = Score 10
      | otherwise      = Score 0
        where c' = toUpper c

-- makeCars :: String -> [Char]
-- makeCars = filter

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score s) = s
