import Data.Text.Internal.Fusion.Size (larger)

fac2 :: Int -> Int
fac2 n = product [1..n]


fac1 :: Int -> Int
fac1 0 = 1
fac1 n = n * fac1(n - 1)

-- Below is definition of * operator. It is primitive in haskell tho.
-- It can be defined using recursion.

-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m k n = m + (m * (n - 1))

-- Library function product with recursion

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (n:ns) = n * product1 ns

-- Lists in haskell are constructed using cons operator
-- [2,3,4] = 2:(3:(4:[]))

length1 :: [a] -> Int
length1 [] = 0
length1 (_:xs) = 1 + length xs


reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = (reverse1 xs) ++ [x]

-- Definition of ++ operator, uses con : operator with recursion
-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

-- Insertion into a sorted list

insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : (insert1 x ys)

-- Insertion sort in haskell Beautiful!
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert1 x (isort xs)


zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = [(x, y)] ++ zip1 xs ys

drop1 :: Int -> [a] -> [a]
drop1 _ [] = []
drop1 0 xs = xs
drop1 n (x:xs) = drop1 (n - 1) xs


qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 smaller ++ [x] ++ qsort1 larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b >= x]

even1 :: Int -> Bool
even1 0 = True
even1 n = odd (n - 1)

odd1 :: Int -> Bool
odd1 0 = False
odd1 n = even (n - 1)

sepeo :: [Int] -> [Int]
sepeo [] = []
sepeo (x:xs)
    | odd1 x = x:(sepeo xs)
    | otherwise = sepeo xs ++ [x]
    