module SumOfMultiples (sumOfMultiples) where


unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs)
  | x `elem` xs = unique xs
  | otherwise = x:unique xs

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (unique (concat multiples))
                where
                  multiples = [[n | n <- [1..limit - 1], n `mod` factor == 0 ] | factor <- factors, factor /= 0]
                  
                  