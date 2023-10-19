
-- List comprehensions


-- {1, 4, 9, 16, 25}
-- | is such that
-- <- is drawn from
-- expression x <- [1..5] is called a generator

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]


firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]


length1 :: [a] -> Int
length1 xs = sum [1 | _ <- xs]


factors1 :: Int -> [Int]
factors1 n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors1 n == [1, n]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']