module Party where

import Data.List
import Data.Monoid
import Data.Tree
import Employee

---------------------------------- Exercise 1 ----------------------------------
----- Part 1 -----
glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs fun) = GL (x : xs) (fun + empFun x)

----- Part 2 -----
instance Monoid GuestList where
    mempty = GL [] 0
    
instance Semigroup GuestList where
    GL xs f1 <> GL ys f2 = GL (xs ++ ys) (f1 + f2)

----- Part 3 -----
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

---------------------------------- Exercise 2 ----------------------------------
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a forest) = f a (treeFold f <$> forest)

---------------------------------- Exercise 3 ----------------------------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss withoutBosses, withBosses)
  where withBosses    = foldMap fst gls
        withoutBosses = foldMap snd gls

---------------------------------- Exercise 4 ----------------------------------
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

---------------------------------- Exercise 5 ----------------------------------

formatEmp :: [Employee] -> String
formatEmp = unlines . sort . fmap empName

formatGL :: GuestList -> String
formatGL (GL xs fun) = "Total fun: " ++ show fun ++ "\n" ++ formatEmp xs

main :: IO()
main = do
  contents <- readFile "company.txt"
  putStr . formatGL . maxFun . read $ contents