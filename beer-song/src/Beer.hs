module Beer (song) where

song :: String
song = foldl1 (++) [verse n | n <- [99,98..0]]
    where
        verse :: Int -> String
        verse n =
            (if n == 0 then "No more bottles of beer" else bottle n) ++
                " on the wall, " ++ bottle n ++ ".\n" ++
            case n of
                0 -> "Go to the store and buy some more, " ++ bottle 99 ++
                    " on the wall."
                1 -> "Take it down and pass it around, " ++ bottle 0 ++
                    " on the wall.\n"
                _ -> "Take one down and pass it around, " ++ bottle (n-1) ++
                    " on the wall.\n"
            ++ "\n"
            where
                bottle :: Int -> String
                bottle 0 = "no more bottles of beer"
                bottle 1 = "1 bottle of beer"
                bottle n = show n ++ " bottles of beer"
