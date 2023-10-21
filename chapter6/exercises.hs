-- Exercise 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n 
    | n < 0 = error "negatives not allowed"
    | n == 0 = 1
    | n > 0 = n * factorial (n - 1)


-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^) :: Integer -> Integer -> Integer
x ^ 0 = 1
1 ^ _ = 1
x ^ y = x * (x Main.^ (y - 1))

-- Exercise 4

euclid :: Int -> Int -> Int
euclid 0 _ = 0
euclid _ 0 = 0
euclid x y
    | x == y = x 
    | x < y = euclid x (y - x)
    | otherwise = euclid (x - y) y

-- Exercise 6

and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs

-- Exercise 7

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs


-- Exercise 8
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x: replicate1 (n - 1) x


(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
[] !! _ = error "Index too large"
(x:xs) !! n = xs Main.!! (n - 1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 val [] = False 
elem1 val (x:xs) 
    | x == val = True
    | otherwise = elem1 val xs


merge1 :: Ord a => [a] -> [a] -> [a]
merge1 [] ys = ys
merge1 xs [] = xs 
merge1 (x:xs) (y:ys)
    | x <= y = x:merge1 xs (y:ys)
    | otherwise = y:merge1 (x:xs) ys



halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take n xs, drop n xs)
        where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge1 (msort ls) (msort rs)
        where (ls, rs) = halve xs


sum1 :: Integral a => [a] -> a  
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

take1 :: Int -> [a] -> [a]
take1 0 xs = xs 
take1 n (x:xs) = x:take1 (n - 1) xs

last1:: [a] -> a
last1 [x] = x
last1 (x:xs) = last1 xs

-- Higher Order Functions

