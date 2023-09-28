import Data.Char

-- Exercise 1

-- [a ^ 2 | a <- [1..100]] using list comprehension
-- Using recursion below

squareSum :: Int -> Int
squareSum 0 = 0
squareSum n = n ^ 2 + squareSum (n - 1)

-- Exercise 2
grid :: Int -> Int -> [(Int, Int)]
-- grid n m = [(a, b) | a <- [0..n], b <- [0..m]]

grid n m = [(a, b) | a <- ns, b <- ms]
            where 
                ns | n < 0 = [n..0] | otherwise = [0..n]
                ms | m < 0 = [m..0] | otherwise = [0..m]

square1 :: Int -> [(Int, Int)]
square1 n = [(a, b) | (a, b) <- grid n n, a /= b]

-- Exercise 4
replicate1 :: Int -> a -> [a]
replicate1 n val = [val | _ <- [0..n-1]]

-- Exercise 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- Exercise 6
factors1 :: Int -> [Int]
factors1 n = [x | x <- [1..n-1], n `mod` x == 0]

isperfect :: Int -> Bool
isperfect n = n == sum (factors1 n)

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isperfect x]


-- Exercise 7 FAILURE
xys = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]


-- Exercise 8 
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions a xs = find a (zip xs [0..length xs])

-- Exercise 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a * b | (a, b) <- zip xs ys]

-- Exercise 10
let2int1 :: Char -> Int
let2int1 ch = ord ch - ord 'a'

int2let1 :: Int -> Char
int2let1 i = chr (i + ord 'a')

let2int2 :: Char -> Int
let2int2 ch = ord ch - ord 'A'

int2let2 :: Int -> Char
int2let2 i = chr (i + ord 'A')


shift :: Int -> Char -> Char
shift n x 
    | x >= 'a' && x <= 'z' = int2let1 ((let2int1(x) + n) `mod` 26)
    | x >= 'A' && x <= 'Z' = int2let2 ((let2int2(x) + n) `mod` 26)
    | otherwise = x

encode1 :: Int -> String -> String
encode1 n xs = [shift n x | x <- xs]