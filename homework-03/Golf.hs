module Golf where

import Data.List

---------------------------------- Exercise 1 ----------------------------------
skips :: [a] -> [[a]]
skips xs = mapInd everyNth $ repeatN xs

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

repeatN :: [a] -> [[a]]
repeatN xs = replicate (length xs) xs

everyNth :: Integral a => [b] -> a -> [b]
everyNth xs nth = map fst $ filter (\p -> snd p `mod` nth == 0) $ orderPairs xs

orderPairs ::  (Num b, Enum b) =>[a] -> [(a, b)]
orderPairs xs = zip xs [1..]

---------------------------------- Exercise 2 ----------------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima = map get2nd .filter isLm.groupsOf3 

isLm :: [Integer] -> Bool
isLm xs = length xs == 3 && (maximum xs == xs!!1)

get2nd :: [Integer] -> Integer
get2nd (_:y:_) = y

groupsOf3 :: [a] -> [[a]]
groupsOf3 = map (take 3).tails

---------------------------------- Exercise 3 ----------------------------------

histogram :: [Integer] -> String
histogram xs = unlines $ reverse.transpose $ map showRow (frequencies xs)
    where maxN = maximum.map snd. frequencies
          showRow (label, num) = (show label) ++ "=" ++ bar num (maxN xs)

bar :: Int -> Int -> String
bar n maxN = take maxN $ replicate n '*' ++ repeat ' '

frequencies :: [Integer] -> [(Integer, Int)]
frequencies xs = map (count xs) [0..9]

count :: [Integer] -> Integer -> (Integer, Int)
count xs x = (x, length $ filter (==x) xs)