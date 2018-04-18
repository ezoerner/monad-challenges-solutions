
import qualified MCPrelude as MC

import HexDecode
import Set1
import qualified Set2 as S2
import Set3

main :: IO ()
main = do
    print ""
    print $ hexDecode "67656E6572616C41203A3A202861202D3E206229202D3E2047656E2061202D3E2047656E2062"
    print $ hexDecode "6D6B47656E203A3A2061202D3E2047656E2061"
    print $ hexDecode "64617461204D617962652061203D204E6F7468696E67207C204A7573742061"
    print $ hexDecode "202073686F77204E6F7468696E67203D20224E6F7468696E6722DA202073686F7720284A757374206129203D20224A7573742022202B2B2073686F772061"
    print $ hexDecode "794C696E6B203A3A202861202D3E2062202D3E206329202D3E204D617962652061202D3E204D617962652062202D3E204D617962652063"
    print $ hexDecode "7472616E734D61796265203A3A202861202D3E206229202D3E204D617962652061202D3E204D617962652062"
    print $ hexDecode "7461696C4D6178203A3A204F72642061203D3E205B615D202D3E204D6179626520284D61796265206129"
    print $ hexDecode "636F6D62696E65203A3A204D6179626520284D61796265206129202D3E204D617962652061"
    print $ hexDecode "636F6D6253746570203A3A205B61202D3E20625D202D3E205B615D202D3E205B625D"

    print "--------- Set 1 -----------"
    print $ fiveRands
    print $ product fiveRands == 8681089573064486461641871805074254223660
    print $ randLetter $ MC.mkSeed 4
    print $ randString3
    let
        a = fst $ MC.rand $ MC.mkSeed 1
        b = fst $ randEven $ MC.mkSeed 1
        c = fst $ randOdd $ MC.mkSeed 1
        d = fst $ randTen $ MC.mkSeed 1
    print $ product [b, c, d] == 189908109902700
    print $ fst (randPair $ MC.mkSeed 1) == ('l',282475249)
    print $ fst (generalPair randLetter MC.rand $ MC.mkSeed 1) == ('l',282475249)
    print $ fst (generalPair2 randLetter MC.rand $ MC.mkSeed 1) == ('l',282475249)
    print $ fst (repRandom (replicate 3 randLetter) $ MC.mkSeed 1) == randString3
    print $ mkGen 42 $ MC.mkSeed 1

    print "--------- Set 2 -----------"

    print $ S2.Just 5
    print $ (S2.Nothing :: S2.Maybe Int)

    print $ (S2.headMay [] :: S2.Maybe Int)
    print $ S2.headMay [1, 2]
    print $ S2.headMay [1]

    print $ (S2.tailMay [] :: S2.Maybe [Int])
    print $ S2.tailMay [1,2]
    print $ S2.tailMay [1]

    print $ (S2.lookupMay 42 [] :: S2.Maybe Int)
    print $ S2.lookupMay 42 [(42, 1)]
    print $ S2.lookupMay 42 [(1, 2)]
    print $ S2.lookupMay 42 [(1, 2), (42, 3)]

    print $ S2.divMay 4 2
    print $ S2.divMay 108 12
    print $ S2.divMay 2 4
    print $ S2.divMay 42 0

    print $ (S2.maximumMay [] :: S2.Maybe Int)
    print $ S2.maximumMay [1]
    print $ S2.maximumMay [42, 3]
    print $ S2.maximumMay [3, 108, 12]
    print $ S2.maximumMay [3, 108, 12, 108]

    print $ (S2.minimumMay [] :: S2.Maybe Int)
    print $ S2.minimumMay [1]
    print $ S2.minimumMay [42, 3]
    print $ S2.minimumMay [108, 3, 12]
    print $ S2.minimumMay [108, 3, 12, 3]

    print $ MC.greekDataA
    print $ MC.greekDataB

    print $ S2.queryGreek MC.greekDataA "beta"

    print $ S2.queryGreek MC.greekDataA "alpha" == S2.Just 2.0
    print $ S2.queryGreek MC.greekDataA "beta" == S2.Nothing
    print $ S2.queryGreek MC.greekDataA "gamma" == S2.Just 3.3333333333333335
    print $ S2.queryGreek MC.greekDataA "delta" == S2.Nothing
    print $ S2.queryGreek MC.greekDataA "zeta" == S2.Nothing

    print $ S2.queryGreek MC.greekDataB "rho" == S2.Nothing
    print $ S2.queryGreek MC.greekDataB "phi" == S2.Just 0.24528301886792453
    print $ S2.queryGreek MC.greekDataB "chi" == S2.Just 9.095238095238095
    print $ S2.queryGreek MC.greekDataB "psi" == S2.Nothing
    print $ S2.queryGreek MC.greekDataB "omega" == S2.Just 24.0

    print $ S2.queryGreek2 MC.greekDataA "beta"

    print $ S2.queryGreek2 MC.greekDataA "alpha" == S2.Just 2.0
    print $ S2.queryGreek2 MC.greekDataA "beta" == S2.Nothing
    print $ S2.queryGreek2 MC.greekDataA "gamma" == S2.Just 3.3333333333333335
    print $ S2.queryGreek2 MC.greekDataA "delta" == S2.Nothing
    print $ S2.queryGreek2 MC.greekDataA "zeta" == S2.Nothing
    print $ S2.queryGreek2 MC.greekDataB "rho" == S2.Nothing
    print $ S2.queryGreek2 MC.greekDataB "phi" == S2.Just 0.24528301886792453
    print $ S2.queryGreek2 MC.greekDataB "chi" == S2.Just 9.095238095238095
    print $ S2.queryGreek2 MC.greekDataB "psi" == S2.Nothing
    print $ S2.queryGreek2 MC.greekDataB "omega" == S2.Just 24.0
    print $ S2.addSalaries salaries "bob" "john"
    print $ S2.addSalaries2 salaries "alice" "carol"
    print $ S2.addSalaries2 salaries "bob" "john"
    print $ S2.yLink (+) (S2.Just 1) (S2.Just 5)
    print $ S2.tailProd [1, 2, 3]
    print $ S2.tailProd [1, 2]
    print $ S2.tailProd [1]
    print $ S2.tailProd []
    print $ S2.tailSum [1, 2, 3]
    print $ S2.tailSum [1, 2]
    print $ S2.tailSum [1]
    print $ S2.tailSum []
    print $ S2.combine (S2.Just (S2.Just 1))
    print $ S2.tailMax [1, 3, 42]
    print $ S2.tailMax [1000, 42]
    print $ S2.tailMax [1000]
    print $ (S2.tailMax [] :: S2.Maybe Int)
    print $ S2.tailMin [5, 42, 1]
    print $ S2.tailMin [1, 42]
    print $ S2.tailMin [1000]
    print $ (S2.tailMin [] :: S2.Maybe Int)
    
    print "--------- Set 3 -----------"
    print $ allPairs1 [1,2] [3,4]
    print $ allPairs1 [1,2,3] [4,5,6]
    print $ allPairs1 [1..3] [6..8]
    print $ allPairs1 [1..3] [6..8] == [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
    print $ allPairs [1,2] [3,4]
    print $ allPairs [1,2,3] [4,5,6]
    print $ allPairs [1..3] [6..8]
    print $ allPairs [1..3] [6..8] == [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
    print $ MC.cardRanks
    print $ MC.cardSuits
    print $ allPairs1 MC.cardRanks MC.cardSuits
    print $ allPairs MC.cardRanks MC.cardSuits
    print $ Card 1 "H"
    print $ show (Card 2 "h") == "2h"
    print $ show (allCards1 MC.cardRanks MC.cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
    print $ show (allCards MC.cardRanks MC.cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
    print $ show (allCards1 MC.cardRanks MC.cardSuits)
    print $ allCombs35 (,,) [1,2] [3,4] [5,6] == [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]

salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]
