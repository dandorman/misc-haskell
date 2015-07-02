{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    ("I" : ts : msg)       -> LogMessage Info (read ts) (unwords msg)
    ("W" : ts : msg)       -> LogMessage Warning (read ts) (unwords msg)
    ("E" : lvl : ts : msg) -> LogMessage (Error $ read lvl) (read ts) (unwords msg)
    _                      -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 <= ts2 = Node (insert msg1 left) msg2 right
  | ts1 >  ts2 = Node left msg2 (insert msg1 right)
insert _ tree = tree -- When omitted, complains about non-exhaustive patterns.

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ (msg : (inOrder right))

getText :: LogMessage -> String
getText (Unknown txt) = txt
getText (LogMessage _ _ txt) = txt

isSevereEnough :: Int -> LogMessage -> Bool
isSevereEnough threshold (LogMessage (Error severity) _ _) = severity >= threshold
isSevereEnough _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = (map getText . filter (isSevereEnough 50) . inOrder . build) msgs
