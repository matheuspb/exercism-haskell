module Bob (responseFor) where

lc = ['a'..'z']
uc = ['A'..'Z']

letters = lc ++ uc

shout :: String -> Bool
shout s = let clean = [x | x <- s, x `elem` letters] in
    all (`elem` uc) clean && not (null clean)

isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r', '\f', '\v']

trim :: String -> String
trim s = if isSpace (last s) then trim (init s) else s

responseFor :: String -> String
responseFor q
    | all isSpace q        = "Fine. Be that way!"
    | shout q              = "Whoa, chill out!"
    | last (trim q) == '?' = "Sure."
    | otherwise            = "Whatever."
