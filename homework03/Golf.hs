module Golf where

import Data.List


skips :: [a] -> [[a]]
skips lst = map (every lst) [1..(length lst)]

every :: [a] -> Int -> [a]
every lst n = map snd (filter (dividableBy n) (zip [1..] lst))

dividableBy :: Int -> (Int, a) -> Bool
dividableBy dividor dividend = mod (fst dividend) dividor == 0


{- Local maxima -}
localMaxima :: [Integer] -> [Integer]
localMaxima []    = []
localMaxima [_]   = []
localMaxima [_,_] = []
localMaxima (x1:x2:x3:xs) = case (x1 < x2) && (x2 > x3) of
                              True  -> x2:localMaxima (x2:x3:xs)
                              False -> localMaxima (x2:x3:xs)


{- Histogram -}
count :: [Integer] -> [Int]
count lst = map (subtract 1) $ map length $ group $ sort $ [0..9] ++ lst

makeStars :: Int -> Int -> String
makeStars max n = (replicate (max - n) ' ') ++ (replicate n '*')

horizontalStars :: [Integer] -> [String]
horizontalStars lst = map (makeStars (maximum (count lst))) (count lst)

histogram :: [Integer] -> String
histogram l = (intercalate "\n" $ transpose $ horizontalStars l)
              ++ "\n==========\n0123456789\n"
