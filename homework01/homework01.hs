-- Validating Credit Card Numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft [x] = [x]
doubleFromLeft (x:y:zs) = x:y*2:doubleFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleFromLeft (reverse l))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n
  | sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0 = True
  | otherwise                                               = False


-- The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1    = [(a, b)]
  | otherwise = hanoi (n-1) a c b ++ (a, b) : hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 1 = [(a,b)]
  | n == 2 = [(a,c), (a,b), (c,b)]
  | n == 3 = [(a,c), (a,d), (a,b), (d,a), (c,a)]
  | otherwise = hanoi4 (n-1) a c b d ++ ((a,b) : hanoi4 (n-1) c b a b)
