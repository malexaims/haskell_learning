{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinListBuffer where

import Data.Monoid
import Control.Monad.State

import Buffer
import JoinList
import Scrabble
import Sized
import Editor

-- Based on the example from the HW and review of Editor.hs
-- The Size represents the line number and
-- Score represents the line's score.
-- Each line will be a Single (score, size) String
makeSingle :: String -> JoinList (Score,Size) String
makeSingle s = Single (sc, sz) s
            where sc = scoreString s
                  sz = Size 1

instance Buffer (JoinList (Score, Size) String) where
  toString           = unlines . jlToList

  fromString s       = foldl (+++) Empty (map makeSingle $ lines s)

  line i j           = indexJ i j

  replaceLine i ln b = fHalf +++ nl +++ sHalf
                        where fHalf = takeJ i b
                              sHalf = dropJ (i+1) b
                              nl = fromString ln

  numLines           = getSize . snd . tag

  value              = getScore . fst . tag


contents :: JoinList (Score, Size) String
contents = fromString $ unlines
           [ "This buffer is for notes you don't want to save, and for"
           , "evaluation of steam valve coefficients."
           , "To load a different file, type the character L followed"
           , "by the name of the file."
           ]

main = runEditor editor $ contents
