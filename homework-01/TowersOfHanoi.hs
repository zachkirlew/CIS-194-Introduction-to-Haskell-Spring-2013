module TowersOfHanoi where

type Peg = String
type Move = (Peg, Peg)

---------------------------------- Exercise 5 ----------------------------------
-- Return the list of moves to be performed to move n discs from the source
-- peg to the destination peg using one auxiliary peg:
--     1. move n−1 discs from src to aux using dst as temporary storage
--     2. move the top disc from src to dst
--     3. move n−1 discs from aux to dst using src as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = aToC ++ aToB ++ bToC
   where
    aToC = hanoi (n - 1) a c b
    aToB = [(a,b)]
    bToC = hanoi (n - 1) c b a