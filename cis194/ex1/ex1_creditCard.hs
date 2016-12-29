toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = remainder : toDigitsRev divResult
    where
      remainder = n `mod` 10
      divResult = n `div` 10

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = foldr (\n acc -> (doubleIfRequired n (length acc)):acc) []
  where doubleIfRequired n l = if odd l then n*2 else n

sumDigitNum :: Integer -> Integer
sumDigitNum n
  | n < 10 = n
  | otherwise = sum $ toDigits n

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigitNum

validate :: Integer -> Bool
validate n
 | (creditCardChecksum n) == 0 = True
 | otherwise = False
    where
      creditCardChecksum = (`mod` 10) . sumDigits . doubleEveryOther . toDigits
