{-# LANGUAGE MonadComprehensions #-}
--{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
  
import MCPrelude  (Seed, rand, mkSeed, toLetter)

fiveRands :: [Integer]
fiveRands = take 5 $ drop 1 $ map fst $ iterate (\(_, seed) -> rand seed) (0, mkSeed 1)
    
type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
randLetter seed = case rand seed of
  (n , seed) -> (toLetter n, seed)

randString3 :: String
randString3 = take 3 $ drop 1 $ map fst  $
  iterate (\(_, seed) -> randLetter seed) ('0', mkSeed 1)


randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA $ (+ 1) . (* 2)

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10)

generalA :: (Integer -> Integer) -> Gen Integer
generalA f seed = case rand seed of
  (n, sd) -> (f n, sd)