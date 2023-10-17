module Grains (square, total) where
square :: Integer -> Maybe Integer
square 1 = Just 1
square n 
    | n <= 0 || n > 64 = Nothing
    | n == 1 = Just 1
    | otherwise = fmap (*2) (square (n - 1))
extractor :: Maybe Integer -> Integer
extractor (Just n) = n
total :: Integer
total = sum [extractor x | x <- [square n | n <- [1..64]]]
