{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where
  
import MCPrelude

-- required for Haskell for Mac to display values in playground
import Prelude (($!)) 

-- 1. Generating combinations
allPairs1 :: [a] -> [b] -> [(a,b)]
allPairs1 as bs =
  let
    nextPair [] _ = []
    nextPair (_ : xs) [] = nextPair xs bs
    nextPair xs@(x : _) (y : ys) = (x, y) : nextPair xs ys
  in
    nextPair as bs
    
    
-- 2. Poker hands
data Card = Card Int String

instance Show (Card) where
  show (Card a b)= show a ++ b
  
allCards1 :: [Int] -> [String] -> [Card]
allCards1 ranks suits =
  let
    nextCard [] _ = []
    nextCard (_ : rs) [] = nextCard rs suits
    nextCard rs@(r : _) (s : ss) = Card r s : nextCard rs ss
  in
    nextCard ranks suits
   

-- 3. Generalizing pairs and cards
allCombs1 :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs1 f as bs =
  let
    next [] _ = []
    next (_ : xs) [] = next xs bs
    next xs@(x : _) (y : ys) = f x y : next xs ys
  in
    next as bs


allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs1 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs1 Card

-- 4. Combinations of three things
allCombs34 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs34 f as bs cs =
  let
    next [] _ _ = []
    next (_ : xs) [] _ = next xs bs cs
    next xs (_ : ys) [] = next xs ys cs
    next xs@(x : _) ys@(y : _) (z : zs) = f x y z : next xs ys zs
  in
    next as bs cs

-- 5. Combinations of more things
combStep :: [a -> b] -> [a] -> [b]
combStep fs as =
  let
    next [] _ = []
    next (_ : fs) [] = next fs as
    next fs@(f : _) (y : ys) = f y : next fs ys
  in
    next fs as
      
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f as bs =
  let
    next [] _ = []
    next (_ : xs) [] = next xs bs
    next xs@(x : _) (y : ys) = f x y : next xs ys
  in
    next as bs
  


