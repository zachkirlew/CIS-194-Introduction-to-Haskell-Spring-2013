---------------------------------- Exercise 1 ----------------------------------

----- 1. -----
fun1' :: [Integer] -> Integer
fun1' = product.map (subtract 2).filter even

----- 2. -----
fun2' :: Integer -> Integer
fun2' = sum.filter even .(takeWhile (/=1) ).iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

---------------------------------- Exercise 2 ----------------------------------
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf 

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h left val right)
    | h1 < h2 = Node h (insert x left) val right
    | h1 > h2 = Node h left val (insert x right)
    | otherwise = Node (h + 1) newLeft val right
    where h1 = height left
          h2 = height right
          newLeft = insert x left

height :: Tree a -> Integer
height Leaf = -1
height (Node n _ _ _) = n

---------------------------------- Exercise 3 ----------------------------------
 
----- 1. -----
xor :: [Bool] -> Bool
xor = foldr (/=) False

----- 2. -----
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


----- 3. -----
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = foldr.flip

---------------------------------- Exercise 4 ----------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((1+).(2*)) $ filter (`notElem` (takeWhile (n>=).map tupleCheck $ cartProd range range) ) range
    where range = [1..n]

tupleCheck :: (Integer,Integer) -> Integer
tupleCheck (i,j) = i + j + (2 * i * j)


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]