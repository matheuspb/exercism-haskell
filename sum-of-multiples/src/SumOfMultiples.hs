module SumOfMultiples (sumOfMultiples) where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy n d = n `mod` d == 0

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors l = sum [x | x <- [1..l-1], any (divisibleBy x) factors]
