module RunLength (decode, encode) where

import Data.Char
import Data.Maybe
import Data.List

decode :: String -> String
decode [] = []
decode s
    | null n    = c : decode rest
    | otherwise = replicate (read n) c ++ decode rest
    where
        (n, r)    = span isDigit s
        (c, rest) = fromJust $ uncons r

encode' :: Int -> String -> String
encode' _ [] = []
encode' acc (x:xs)
    | null xs      = encoded
    | x == head xs = encode' (acc + 1) xs
    | otherwise    = encoded ++ encode' 1 xs
    where
        encoded = (if acc == 1 then [] else show acc) ++ [x]

encode :: String -> String
encode = encode' 1
