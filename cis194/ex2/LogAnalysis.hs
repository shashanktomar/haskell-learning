module LogAnalysis where
import Log

-- parseMessage "This is not in the right format"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords:: [String] -> LogMessage
parseWords ("I":t:s) = LogMessage Info (read t) (unwords s)
parseWords ("W":t:s) = LogMessage Warning (read t) (unwords s)
parseWords ("E":p:t:s) = LogMessage (Error (read p)) (read t) (unwords s)
parseWords ws = Unknown (unwords ws)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node l n r)
  | before m n = Node (insert m l) n r
  | otherwise = Node l n (insert m r)
  where
    before (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 <= t2

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ (m : inOrder r)

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error p) _ _) = p >= 50
isSevereError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extractMsg) . inOrder . build . (filter highPriorityMsg)
  where
    highPriorityMsg (LogMessage (Error p) _ _) = p >= 50
    highPriorityMsg _ = False
    extractMsg (LogMessage _ _ m) = m
