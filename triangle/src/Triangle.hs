module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
    | (a >= b + c) || (b >= c + a) || (c >= a + b) = Illegal
    | (a == b) && (a == c)             = Equilateral
    | (a == b) || (a == c) || (b == c) = Isosceles
    | otherwise                        = Scalene
