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
randPair :: Gen (Char, Integer)
randPair seed = ((ch, int), newSeed)
  where
    (ch, chSeed) = randLetter seed
    (int, newSeed) = rand chSeed
    
generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair genA genB seed = ((a, b), newSeed)
  where
    (a, aSeed) = genA seed
    (b, newSeed) = genB aSeed
    
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB ab2c genA genB seed = (c, newSeed)
  where
    (a, aSeed) = genA seed
    (b, bSeed) = genB aSeed
    (c, newSeed) = ((ab2c a b), bSeed)
    
generalPair2:: Gen a -> Gen b -> Gen (a,b)
generalPair2 = generalB (,)

-- 5. Generalizing Lists of Generators
repRandom :: [Gen a] -> Gen [a]
--repRandom :: [Seed -> (a, Seed)] -> (Seed -> ([a], Seed))
repRandom genAs seed0 = case foldl f ([], seed0) genAs of
    (as, resultSeed) -> (reverse as, resultSeed)
  where
    f (as, seed) genA  = case genA seed of
      (a, sd) -> (a : as, sd)
   
-- 6. Threading the random number state
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo genA k seed = case genA seed of
  (a, sd) -> k a sd
  
mkGen :: a -> Gen a
mkGen a = \seed -> (a, seed)
