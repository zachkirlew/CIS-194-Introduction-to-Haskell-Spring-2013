module ValidateCreditCard where


---------------------------------- Exercise 1 ----------------------------------
-- Convert an integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits int
   | int > 0 = toIntegerList int
   | otherwise = []

toIntegerList :: Integer -> [Integer]
toIntegerList = map ( read . (:[])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse.toDigits

---------------------------------- Exercise 2 ----------------------------------
-- Double every other number 
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = zipWith (*) n (cycle [1,2])

---------------------------------- Exercise 3 ----------------------------------
-- Returns the sum of all digits of each number of the list
sumDigits :: [Integer] -> Integer
sumDigits list = sum $ map (sum.toIntegerList) list

---------------------------------- Exercise 4 ----------------------------------
-- Indicates whether or not a credit card number is valid
validate :: Integer -> Bool
validate = isValid.sumDigits.reverse.doubleEveryOther.toDigitsRev

isValid :: Integer -> Bool
isValid value = value `mod` 10 == 0