-- Problem 1


-- ['a', 'b', 'c']
-- [Char]
-- ('a','b','c')
-- (Char, Char, Char)
-- [(False, '0'), (True, '1')]
-- [(Bool, Char)]
-- ([False, True], ['0','1'])
-- ([Bool], [Char])
-- [tail, init, reverse]
-- [[a] -> [a]]

-- Problem 2

bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1,2,3],[1,2]]

add1 :: Int -> Int -> Int -> Int
add1 x y z = x + y + z

copy1 :: a -> (a, a)
copy1 x = (x, x)

apply1 :: (a -> b) -> a -> b
apply1 f x = f x

hx1 :: Int -> Bool
hx1 1 = False


-- Problem 3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- Infinite possible arguments
-- Hence, difficult to test on all test cases
-- for Eq methods