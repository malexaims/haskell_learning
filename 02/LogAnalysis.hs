{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

major :: Int -> LogMessage -> Bool
major thres (LogMessage (Error l) _ _)
    | (read (show l) >= thres)  = True
    | otherwise                 = False
major _ _ = False

parseMessage :: String -> LogMessage
parseMessage str =
  let message = words str
  in 
    case message of
      ("E":sLevel:tStamp:rest) ->
        LogMessage (Error (read sLevel)) (read tStamp) (unwords rest)
      ("I":tStamp:rest) ->
        LogMessage Info (read tStamp) (unwords rest)
      ("W":tStamp:rest) ->
        LogMessage Warning (read tStamp) (unwords rest)
      _ ->
        Unknown (unwords message)


parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ time1 _) (Node leftTree m2@(LogMessage _ time2 _) rightTree)
    | (time1 > time2) = Node leftTree m2 (insert m1 rightTree)
    | otherwise = Node (insert m1 leftTree) m2 rightTree
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = error "Cannot build from empty list."
build [m] = insert m Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node treeLeft message treeRight) =
  inOrder treeLeft ++ [message] ++ inOrder treeRight

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = [cleanMessage m | m <- sorted, (major 50 m)]
    where sorted = inOrder (build messages)

cleanMessage :: LogMessage -> String
cleanMessage (LogMessage _ _ msg) = show msg
cleanMessage _ = error "No message to clean"
