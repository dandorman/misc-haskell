-- Exercise 1

stringToDigits :: String -> [Integer]
stringToDigits = map (\x -> read [x] :: Integer)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = stringToDigits (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' []       = []
doubleEveryOther' [n]      = [n]
doubleEveryOther' (n:m:ns) = n  : (m * 2) : doubleEveryOther' ns

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits n = sum (map (sum . toDigits) n)

-- Exercise 4

validate :: [Integer] -> Bool
validate n = (sumDigits n) `mod` 10 == 0
