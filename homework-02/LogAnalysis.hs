module LogAnalysis where

import Log

---------------------------------- Exercise 1 ----------------------------------
parseMessage :: String -> LogMessage
parseMessage = parseWords.words

parseWords :: [String] -> LogMessage
parseWords ("E":severity:time:message) = LogMessage (Error $ read severity) (read time) (unwords message)
parseWords ("W":time:message) = LogMessage Warning (read time) (unwords message)
parseWords ("I":time:message) = LogMessage Info (read time) (unwords message)
parseWords string = Unknown (unwords string) 

parse :: String -> [LogMessage]
parse input = map parseMessage $ lines input

-- Testing exercise 1
exercise1 = do
    print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
    print $ parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
    print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
    testParse parse 10 "error.log"

---------------------------------- Exercise 2 ----------------------------------
insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert msg1@(LogMessage _ time1 _) (Node left msg2@(LogMessage _ time2 _) right)
    | time1 < time2  = Node (insert msg1 left) msg2 right
    | time1 > time2  = Node left msg2 (insert msg1 right)
insert (Unknown _) messageTree = messageTree

---------------------------------- Exercise 3 ----------------------------------
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

---------------------------------- Exercise 4 ----------------------------------
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

---------------------------------- Exercise 5 ----------------------------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs =  map getErrMessage $ inOrder.build $ filter isRelevant logMsgs

getErrMessage :: LogMessage -> String
getErrMessage (LogMessage _ _ message) = message

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = severity > 50
isRelevant _ = False

-- Testing exercise 5
exercise5 = testWhatWentWrong parse whatWentWrong "sample.log"