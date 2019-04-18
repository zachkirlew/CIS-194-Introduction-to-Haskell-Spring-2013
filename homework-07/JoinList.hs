{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Sized
import Scrabble
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


---------------------------------- Exercise 1 ----------------------------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

---------------------------------- Exercise 2 ----------------------------------
getSizeTag :: (Monoid b, Sized b) => JoinList b a -> Int
getSizeTag = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                   = Nothing
indexJ i _  | i < 0              = Nothing
indexJ i jl | i >= getSizeTag jl = Nothing
indexJ _ (Single _ a)            = Just a
indexJ i (Append _ jl1 jl2)
  | i < left                     = indexJ i jl1
  | otherwise                    = indexJ (i - left) jl2
  where left = getSizeTag jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                   = Empty
dropJ n jl | n <= 0             = jl
dropJ n jl | n >= getSizeTag jl = Empty
dropJ _ (Single _ _)            = Empty
dropJ n (Append _ jl1 jl2)
  | n < left                    = dropJ n jl1 +++ jl2
  | otherwise                   = dropJ (n - left) jl2
  where left = getSizeTag jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                   = Empty
takeJ n _  | n <= 0             = Empty
takeJ n jl | n >= getSizeTag jl = jl
takeJ _ jl@(Single _ _)         = jl
takeJ n (Append _ jl1 jl2)
  | n < left                    = takeJ n jl1
  | otherwise                   = jl1 +++ takeJ (n - left) jl2
  where left = getSizeTag jl1

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

-- Convert a JoinList into a list ignoring monoidal annotations (provided)
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

---------------------------------- Exercise 3 ----------------------------------
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

---------------------------------- Exercise 4 ----------------------------------
instance Monoid m => Monoid (JoinList m a) where
  mempty  = Empty
  mappend = (+++)

instance Buffer (JoinList (Score, Size) String) where
  toString          = unlines . jlToList
  fromString        = mconcat . fmap createJoinList . lines
    where createJoinList s = Single (scoreString s, Size 1) s
  line              = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b
  numLines          = getSizeTag
  value             = getScore . fst . tag

reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
reify = id