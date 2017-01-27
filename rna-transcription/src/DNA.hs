module DNA (toRNA) where

import Data.Maybe (isNothing, fromJust)

toRNA :: String -> Maybe String
toRNA [] = Just []
toRNA (x:xs)
    | isNothing $ transcript x = Nothing
    | otherwise = let ts = toRNA xs in
        if isNothing ts
            then Nothing
            else Just $ fromJust (transcript x) : fromJust ts
    where
        transcript :: Char -> Maybe Char
        transcript 'G' = Just 'C'
        transcript 'C' = Just 'G'
        transcript 'T' = Just 'A'
        transcript 'A' = Just 'U'
        transcript _ = Nothing
