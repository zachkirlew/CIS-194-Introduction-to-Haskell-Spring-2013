{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

---------------------------------- Exercise 1 ----------------------------------
eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

---------------------------------- Exercise 2 ----------------------------------
evalStr :: String -> Maybe Integer
evalStr = fmap eval.parseExp ExprT.Lit ExprT.Add ExprT.Mul

---------------------------------- Exercise 3 ----------------------------------
class Expr a where
    lit :: Integer -> a 
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

---------------------------------- Exercise 4 ----------------------------------
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)
    
instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

---------------------------------- Exercise 5 ----------------------------------
instance Expr Program where
    lit a = [StackVM.PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

---------------------------------- Exercise 6 ----------------------------------
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a _ = Just a
    add a b m = (+) <$> a m <*> b m
    mul a b m = (*) <$> a m <*> b m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer)-> Maybe Integer
withVars vs exp = exp $ M.fromList vs