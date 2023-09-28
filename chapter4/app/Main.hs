module Main where

even1 :: Integral a => a -> Bool
even1 n = n `mod` 2 == 0

splitListAt :: Int -> [a] -> ([a], [a])
splitListAt n xs = (take n xs, drop n xs)

recip1 :: Fractional a => a -> a
recip1 n = 1 / n

abs1 :: Int -> Int
abs1 n = if n >= 0 then n else - n

signum1 :: Int -> Int
signum1 n | n < 0 = -1
        | n == 0 = 0
        | otherwise = 1


main :: IO ()
main = putStrLn "Hello, Haskell!"
