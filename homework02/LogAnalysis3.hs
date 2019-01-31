{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative (liftA)

parseMessage :: String -> LogMessage
parseMessage m =
  case (words m) of
    -- "I":t:ws   -> undefined
    -- "W":t:ws   -> undefined
    -- "E":s:t:ws -> undefined
    _          -> Unknown m

parseInt :: String -> Maybe Int
parseInt s =
  case (reads s :: [(Int, String)]) of
    [(n, "")] -> Just n
    _         -> Nothing

-- info' :: String -> Int -> LogMessage
-- info' c t = LogMessage Info t c

-- info :: String -> Maybe Int -> Maybe LogMessage
-- info _ Nothing    = Nothing
-- info c (Just t)   = Just (LogMessage Info t c)

info :: String -> Int -> LogMessage
info c t    = LogMessage Info t c
-- warning c t = LogMessage Warning t c
-- err c s t   = LogMessage (Error s) t c

-- asInfo :: String -> Maybe Int -> Maybe LogMessage
-- asInfo _ Nothing = Nothing
-- asInfo c (Just t) = Just (info c t)

asInfo c = liftA $ info c
