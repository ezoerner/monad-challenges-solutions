module HexDecode (hexDecode) where

import Numeric (readHex)
import Data.List.Split (chunksOf)

hexDecode :: String -> String
hexDecode = map (toEnum . fst . head . readHex) . (chunksOf 2)