module ETL (transform) where

import Data.Map (Map, empty, toList, insert)

toLowerCase :: Char -> Char
toLowerCase c
    | isUpper c = repeat succ c 32
    | otherwise = c
    where
        isUpper = (`elem` ['A'..'Z'])
        repeat f a 1 = f a
        repeat f a n = f $ repeat f a (n-1)

transform' :: [(a, String)] -> Map Char a -> Map Char a
transform' [] m = m
transform' ((k, []):xs) m = transform' xs m
transform' ((k, v:vs):xs) m =
    transform' ((k, vs):xs) $ insert (toLowerCase v) k m

transform :: Map a String -> Map Char a
transform m = transform' (toList m) empty
