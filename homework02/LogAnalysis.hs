{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = parseInfo xs
parseMessage ('W':' ':xs) = parseWarning xs
parseMessage ('E':' ':xs) = parseError xs
parseMessage msg          = Unknown msg

parseInfo' :: TimeStamp -> String -> LogMessage
parseInfo' t []       = LogMessage Info t ""
parseInfo' t s@(c:cs) = case c of
                          '0' -> parseInfo' (t * 10    ) cs
                          '1' -> parseInfo' (t * 10 + 1) cs
                          '2' -> parseInfo' (t * 10 + 2) cs
                          '3' -> parseInfo' (t * 10 + 3) cs
                          '4' -> parseInfo' (t * 10 + 4) cs
                          '5' -> parseInfo' (t * 10 + 5) cs
                          '6' -> parseInfo' (t * 10 + 6) cs
                          '7' -> parseInfo' (t * 10 + 7) cs
                          '8' -> parseInfo' (t * 10 + 8) cs
                          '9' -> parseInfo' (t * 10 + 9) cs
                          ' ' -> LogMessage Info t cs
                          _   -> LogMessage Info t s

parseInfo :: String -> LogMessage
parseInfo s = parseInfo' 0 s

parseWarning' :: TimeStamp -> String -> LogMessage
parseWarning' t []     = LogMessage Warning t ""
parseWarning' t s@(c:cs) = case c of
                             '0' -> parseWarning' (t * 10    ) cs
                             '1' -> parseWarning' (t * 10 + 1) cs
                             '2' -> parseWarning' (t * 10 + 2) cs
                             '3' -> parseWarning' (t * 10 + 3) cs
                             '4' -> parseWarning' (t * 10 + 4) cs
                             '5' -> parseWarning' (t * 10 + 5) cs
                             '6' -> parseWarning' (t * 10 + 6) cs
                             '7' -> parseWarning' (t * 10 + 7) cs
                             '8' -> parseWarning' (t * 10 + 8) cs
                             '9' -> parseWarning' (t * 10 + 9) cs
                             ' ' -> LogMessage Warning t cs
                             _   -> LogMessage Warning t s

parseWarning :: String -> LogMessage
parseWarning s = parseWarning' 0 s

parseError' :: Int -> Int -> TimeStamp -> String -> LogMessage
parseError' 0 l t s@(c:cs) = case c of
                               '0' -> parseError' 0 (l * 10    ) t cs
                               '1' -> parseError' 0 (l * 10 + 1) t cs
                               '2' -> parseError' 0 (l * 10 + 2) t cs
                               '3' -> parseError' 0 (l * 10 + 3) t cs
                               '4' -> parseError' 0 (l * 10 + 4) t cs
                               '5' -> parseError' 0 (l * 10 + 5) t cs
                               '6' -> parseError' 0 (l * 10 + 6) t cs
                               '7' -> parseError' 0 (l * 10 + 7) t cs
                               '8' -> parseError' 0 (l * 10 + 8) t cs
                               '9' -> parseError' 0 (l * 10 + 9) t cs
                               ' ' -> parseError' 1 l            t cs
                               _   -> parseError' 1 l            t s
parseError' 1 l t s@(c:cs) = case c of
                               '0' -> parseError' 1 l (t * 10    ) cs
                               '1' -> parseError' 1 l (t * 10 + 1) cs
                               '2' -> parseError' 1 l (t * 10 + 2) cs
                               '3' -> parseError' 1 l (t * 10 + 3) cs
                               '4' -> parseError' 1 l (t * 10 + 4) cs
                               '5' -> parseError' 1 l (t * 10 + 5) cs
                               '6' -> parseError' 1 l (t * 10 + 6) cs
                               '7' -> parseError' 1 l (t * 10 + 7) cs
                               '8' -> parseError' 1 l (t * 10 + 8) cs
                               '9' -> parseError' 1 l (t * 10 + 9) cs
                               ' ' -> LogMessage (Error l) t cs
                               _   -> LogMessage (Error l) t s
parseError' _ l t _       = LogMessage (Error l) t ""

parseError :: String -> LogMessage
parseError s = parseError' 0 0 0 s

parse' :: [String] -> [LogMessage]
parse' [] = []
parse' (x:xs) = parseMessage x : parse' xs

parse :: String -> [LogMessage]
parse s = parse' (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t _) tree =
  case tree of
    Leaf -> Node Leaf m Leaf
    Node l mm@(LogMessage _ tt _) r | tt > t  -> Node (insert m l) mm r
    Node l mm@(LogMessage _ tt _) r | tt < t  -> Node l mm (insert m r)
    Node _    (LogMessage _ tt _) _ | tt == t -> tree
    Node _    (LogMessage _ _ _)  _           -> tree
    Node _    (Unknown _) _                   -> tree

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

concatenate :: [a] -> [a] -> [a]
concatenate [] ys = ys
concatenate (x:xs) ys = x:concatenate xs ys

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = concatenate (inOrder l) (m:inOrder r)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = case inOrder (build logs) of
                       [] -> []
                       x:xs -> case x of
                                 LogMessage (Error severity) _ s | severity >= 50 ->
                                                                   s:whatWentWrong xs
                                 _ -> whatWentWrong xs
