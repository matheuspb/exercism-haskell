module Raindrops (convert) where

convert :: Integer -> String
convert n
    | n `divisibleBy` 105 = "PlingPlangPlong"
    | n `divisibleBy` 35  = "PlangPlong"
    | n `divisibleBy` 21  = "PlingPlong"
    | n `divisibleBy` 15  = "PlingPlang"
    | n `divisibleBy` 7   = "Plong"
    | n `divisibleBy` 5   = "Plang"
    | n `divisibleBy` 3   = "Pling"
    | otherwise = show n
    where
        divisibleBy :: Integer -> Integer -> Bool
        divisibleBy n = (== 0) . (n `mod`)
