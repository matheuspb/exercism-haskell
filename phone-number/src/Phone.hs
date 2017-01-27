module Phone (areaCode, number, prettyPrint) where

import Data.Maybe (isNothing, fromJust)


areaCode :: String -> Maybe String
areaCode n
    | isNothing cn = Nothing
    | otherwise    = Just . take 3 $ fromJust cn
    where cn = number n

number :: String -> Maybe String
number n
    | size < 10 || size > 11 = Nothing
    | size == 10             = Just $ cn
    | head cn == '1'         = Just . tail $ cn
    | otherwise              = Nothing
    where
        cn = filter (`elem` ['0'..'9']) n
        size = length cn

prettyPrint :: String -> Maybe String
prettyPrint n
    | isNothing cn = Nothing
    | otherwise    = Just . pretty $ fromJust cn
    where
        cn = number n
        pretty n = "(" ++ (take 3 n) ++ ") " ++ (take 3 $ drop 3 n) ++ "-" ++
            (take 4 $ drop 6 n)
