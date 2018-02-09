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
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA ((+ 1) . (* 2)) rand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand
  
generalA :: (a -> b) -> Gen a -> Gen b
generalA a2b genA seed = case genA seed of
  (a, sd) -> (a2b a, sd)

-- 4. Generalizing Random Pairs
