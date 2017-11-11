module Utils.Strings
( string2int
) where

import Data.Char(ord)

string2int :: [Char] -> Int
string2int = let zero = (ord '0')
                 count = \(i, n) z -> z + n * (10 ^ i)
             in (foldr count 0) . (zip [0..]) . (map (subtract zero)) . (map ord)
