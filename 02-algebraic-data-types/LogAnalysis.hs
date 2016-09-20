{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
  "I":ts:body ->  LogMessage Info (read ts :: TimeStamp) (unwords body)
  "W":ts:body ->  LogMessage Warning (read ts :: TimeStamp) (unwords body)
  "E":code:ts:body -> LogMessage (Error (read code :: Int)) (read ts :: TimeStamp) (unwords body)
  _ -> Unknown msg


parse :: String -> [LogMessage]
parse str = parseLines (lines str)
  where parseLines []     = []
        parseLines (x:xs) = parseMessage x : parseLines xs


-- testParse parse 10 "error.log"


-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ (Node _ (Unknown _) _) = error "The MessageTree is not supposed to contain Unknwon messages"
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left rootMsg@(LogMessage _ rootTs _) right)
  | ts < rootTs = Node (insert msg left) rootMsg right
  | otherwise   = Node left rootMsg (insert msg right)


-- Exercise 3

build :: [LogMessage] -> MessageTree
build msgs = buildTree msgs Leaf
  where buildTree (x:xs) tree = buildTree xs (insert x tree)
        buildTree [] tree     = tree


-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
inOrder Leaf = []


-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = severeBodies (inOrder (build msgs))


severeBodies :: [LogMessage] -> [String]
severeBodies (x:xs)
  | isSevere x = messageBody x : severeBodies xs
  | otherwise  = severeBodies xs
severeBodies [] = []


isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity >= 50
isSevere _ = False


messageBody :: LogMessage -> String
messageBody (Unknown msg) = msg
messageBody (LogMessage _ _ msg) = msg


-- testWhatWentWrong parse whatWentWrong "sample.log"
