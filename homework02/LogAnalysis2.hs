{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessageType :: String -> (MessageType, String)
parseMessageType (c:' ':cs) = case c of
                                'I' -> (Info,    cs)
                                'W' -> (Warning, cs)
                                'E' -> (Error (fst (parseInt cs)), (snd (parseInt cs)))
                                _   -> error "Bad format"
parseMessageType _ = error "No match"

parseTimeStamp :: String -> (TimeStamp, String)
parseTimeStamp = parseInt

parseMessage :: String -> LogMessage
parseMessage s = case fst (parseMessageType s) of
                   -- error _ -> Unknown (snd (parseMessageType s))
                   t -> LogMessage t (fst (parseTimeStamp (snd (parseMessageType s)))) (snd (parseMessageType s))

------------------------------------------

parseInt :: String -> (Int, String)
parseInt s = parseInt' 0 s

parseInt' :: Int -> String -> (Int, String)
parseInt' n [] = (n, [])
parseInt' n (c:cs) = case c of
                       '0' -> parseInt' (n * 10    ) cs
                       '1' -> parseInt' (n * 10 + 1) cs
                       '2' -> parseInt' (n * 10 + 2) cs
                       '3' -> parseInt' (n * 10 + 3) cs
                       '4' -> parseInt' (n * 10 + 4) cs
                       '5' -> parseInt' (n * 10 + 5) cs
                       '6' -> parseInt' (n * 10 + 6) cs
                       '7' -> parseInt' (n * 10 + 7) cs
                       '8' -> parseInt' (n * 10 + 8) cs
                       '9' -> parseInt' (n * 10 + 9) cs
                       _   -> (n, cs)
