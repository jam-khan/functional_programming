import Data.Time.Format.ISO8601 (yearFormat)

-- -- Exercise 1

-- factorial1 :: Int -> Int
-- factorial1 0 = 1
-- factorial1 n 
--     | n > 0 = n * factorial1(n - 1)
--     | n < 0 = -n * factorial1(n + 1)

-- Exercise 2

-- sumdown :: Int -> Int
-- sumdown 0 = 0
-- sumdown n = n + sumdown(n - 1)

-- Exercise 3

-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + m * (n - 1)

-- (^) :: Int -> Int -> Int
-- m ^ n = 0
-- m ^ n = m * (m ^ (n - 1))

-- Exercise 4
euclid :: Int -> Int -> Int
euclid _ 1 = 1
euclid 1 _ = 1
euclid x y
    | x == y = x
    | x > y = euclid (x - y) y
    | otherwise = euclid x (y - x)

-- Exercise 6
and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs)
    |   not x = False
    |   otherwise = and1 xs

concat1 :: [a] -> [a]
concat1 [] = []
concat1 (x:xs) = x:(concat1 xs)