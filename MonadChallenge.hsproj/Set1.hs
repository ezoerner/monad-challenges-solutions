{-# LANGUAGE MonadComprehensions #-}
--{-# LANGUAGE RebindableSyntax  #-}

module Set1 where
  
import MCPrelude (Seed, rand, mkSeed, toLetter)

fiveRands :: [Integer]
fiveRands = take 5 $ drop 1 $ map fst $ iterate (\(_, seed) -> rand seed) (0, mkSeed 1)
    

randLetter :: Seed -> (Char, Seed)
randLetter seed = case rand seed of
  (n , seed) -> (toLetter n, seed)

randString3 :: String
randString3 = take 3 $ drop 1 $ map fst  $
  iterate (\(_, seed) -> randLetter seed) ('0', mkSeed 1)
