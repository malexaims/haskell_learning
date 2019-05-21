{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Scrabble
import Sized

-- Based on the example from the HW and review of Editor.hs
-- The Size represents the line number and
-- Score represents the line's score.
-- Each line will be a Single (score, size) String
makeSingle :: String -> JoinList (Score,Size) String
makeSingle s = Single (sc, sz) s
            where sc = scoreString s
                  sz = Size 1

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList 
  fromString s = foldl (+++) Empty (map makeSingle $ lines s)
