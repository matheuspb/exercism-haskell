module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f [] = []
discard f (x:xs)
    | f x       = discard f xs
    | otherwise = x : discard f xs

keep :: (a -> Bool) -> [a] -> [a]
keep f = discard (not . f)
