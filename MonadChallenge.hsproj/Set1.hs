{-# LANGUAGE MonadComprehensions #-}
--{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
  
import MCPrelude  (Seed, rand, mkSeed, toLetter)

-- 1. Random Number Generation
fiveRands :: [Integer]
fiveRands = take 5 $ drop 1 $ map fst $ iterate (\(_, seed) -> rand seed) (0, mkSeed 1)
    
type Gen a = Seed -> (a, Seed)

-- 2 Random Character Generation
randLetter :: Gen Char
randLetter seed = case rand seed of
  (n , seed) -> (toLetter n, seed)

randString3 :: String
randString3 = take 3 $ drop 1 $ map fst  $
  iterate (\(_, seed) -> randLetter seed) ('0', mkSeed 1)

-- 3. More Generators
randEven :: Gen Integer -- the output of rand * 2
randEven = generalA id (* 2)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA id $ (+ 1) . (* 2)

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA id (* 10)

generalA :: (a -> a) -> (Integer -> a) -> Gen a
generalA f fromInt seed = case rand seed of
  (n, sd) -> (f $ fromInt n, sd)
  
generalA' :: (a -> b) -> Gen a -> Gen b
generalA' a2b genA seed = undefined -- ??

-- 4. Generalizing Random Pairs
