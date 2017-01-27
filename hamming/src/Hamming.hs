module Hamming (distance) where

zipIf :: (a -> a -> Bool) -> [a] -> [a] -> [(a, a)]
zipIf f [] [] = []
zipIf f (a:as) (b:bs) = let next = if f a b then [(a, b)] else [] in
    next ++ zipIf f as bs

distance :: String -> String -> Maybe Integer
distance a b =
    if (length a) == (length b) then
        Just . toInteger . length $ zipIf (/=) a b
    else
        Nothing
