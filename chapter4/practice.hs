
-- even1 :: Integral a => a -> Bool
even1 :: Integral a => a -> Bool
even1 x = x `mod` 2 == 0

splitAt1 :: Int -> [a] -> ([a], [a])
splitAt1 n xs = (take n xs, drop n xs)

recip1 :: Fractional a => a -> a
recip1 n = 1 / n

abs1 :: Int -> Int
abs1 n = if n >= 0 then n else -n

signum1 :: Int -> Int
signum1 n = if n < 0 then -1 else
    if n == 0 then 0 else 1

-- In Haskell, conditional expressions must always have an else branch
-- this avoids well-known dangling else problem

-- Guarded equations
-- guards

abs2 :: Int -> Int
abs2 n | n >= 0 = n
       | otherwise = -n

-- In prelude, simply otherwise means True
signum2 :: (Ord a1, Num a1, Num a2) => a1 -> a2
signum2 n | n < 0 = -1
          | n == 0 = 0
          | otherwise = 1

not1 :: Bool -> Bool
not1 False = True
not1 True = False

(^^^) :: Bool -> Bool -> Bool
True ^^^ b = b
False ^^^ _ = False

-- Below definition is wrong
-- This suggests that if 2 arguments are equal
-- then, result is same as the argument
-- However, Haskell doesn't allows repetition of
-- argument names

-- (&&) :: Bool -> Bool -> Bool
-- b && b = b
-- _ && _ = False

(&&) :: Bool -> Bool -> Bool
b && c | b == c = b
       | otherwise = False

-- Tuple patterns
fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

-- List patterns
-- test :: [Char] -> Bool
-- test ['a', _, _] = True
-- test _ = False

test :: [Char] -> Bool
test ('a':_) = True
test _ = False


head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

-- Function application has the highest priority in Haskell
-- hence, cons operator must be parenthesised

-- head x:_ = x
-- would mean below which is wrong
-- (head x):_ = x

-- In Haskell, lists are not primitive
-- Instead, lists are constructed one element at at a time
-- using cons operator

-- Labmda expressions

-- Lambda expressions are nameless functions
-- \x -> x + x

-- Curried functions formalization

add2 :: Int -> Int -> Int
-- add2 x y = x + y
add2 = \x -> (\y -> x + y)

const1 :: a -> (b -> a)
const1 x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> 2 * x + 1) [0..n-1]

-- instead of 
-- odds n = map (\x -> x * 2 + 1) [0..n-1]

-- (#) :: t1 -> t2 -> t3
-- (#) = \x -> (\y -> x # y)

(x #) = \y -> x # y
-- (# y) = \x -> x # y