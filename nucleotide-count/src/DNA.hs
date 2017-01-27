module DNA (count, nucleotideCounts) where

import Data.Map (Map, fromList)
import Data.Either (rights)

right :: Either a b -> b
right x = head $ rights [x]

nucleotides = "ACGT"

isDNA :: String -> Bool
isDNA strand = all (`elem` nucleotides) strand

count' :: (Eq a) => Int -> a -> [a] -> Int
count' n c [] = n
count' n c (x:xs) =
    if c == x
        then count' (n+1) c xs
        else count' n c xs

count :: Char -> String -> Either String Int
count nucleotide strand =
    if isDNA strand && isDNA [nucleotide]
        then Right $ count' 0 nucleotide strand
        else Left "invalid strand or nucleotide"

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts strand =
    if isDNA strand
        then Right $ Data.Map.fromList
            [(k, right $ count k strand) | k <- nucleotides]
        else Left "invalid strand"
