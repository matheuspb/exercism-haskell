module RunLength (decode, encode) where

decode :: String -> String
decode _ = "You need to implement this function."

encode' :: Int -> String -> String
encode' acc (x:[]) = (if acc == 1 then [] else show acc) ++ [x]
encode' acc (x:xs)
    | x == head xs = encode' (acc + 1) xs
    | otherwise    =
        (if acc == 1
            then []
            else (show acc))
        ++ [x] ++ encode' 1 xs

encode :: String -> String
encode text = encode' 1 text
