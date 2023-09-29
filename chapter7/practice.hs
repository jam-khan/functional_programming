import Distribution.Simple.Utils (xargs)
import Text.XHtml (base)
import Data.Void (vacuous)
twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- Below is map definition using list comprehension
-- Why b? Because it allows for change in types.
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

-- Below is map definition using recursive definition

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map f xs

-- In filter, type should remain same. Hence, takes [a] and returns [a]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f xs = [x | x <- xs, f x]


-- Recursive definition

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs)    | p x = x: filter2 p xs
                    | otherwise = filter2 p xs


sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

all1 :: (a -> Bool) -> [a] -> Bool
all1 f xs = and [f x | x <- xs]

any1 :: (a -> Bool) -> [a] -> Bool
any1 f xs = or [f x | x <- xs]


takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f [] = []
takeWhile1 f (x:xs) 
    | f x = x:takeWhile1 f xs
    | otherwise = []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 f [] = []
dropWhile1 f (x:xs)
    | f x = dropWhile1 f xs
    | otherwise = x:xs



-- Functions that can be represented as following
-- f [] = v
-- f (x:xs) = x # f xs

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum xs

product1 :: Num a => [a] -> a
product1 [] = 1
product1 (x:xs) = x * product xs


sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

product2 :: Num a => [a] -> a
product2 = foldr (*) 1

or1 :: [Bool] -> Bool
or1 = foldr (||) False

and1 :: [Bool] -> Bool
and1 = foldr (&&) True

sum3 :: (Foldable t, Num b) => t b -> b
sum3 xs = foldr (+) 0 xs

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

-- foldr2 (+) 0 [1,2,3]
-- 1 : (2 : (3 : []))
-- 1 + (2 + (3 + 0))


-- Recursion
length1 :: [a] -> Int
length1 = foldr (\_ n -> 1 + n) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse3 :: [a] -> [a]
reverse3 = foldr snoc []

-- (((0 + 1) + 2) + 3)

sum4 :: Num a => [a] -> a
sum4 = sum' 0
        where
            sum' v [] = v
            sum' v (x:xs) = sum' (v+x) xs

-- foldr (#) v [x0, x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
-- foldl (#) v [x0, x1,...,xn] = (... ((v # x0) # x1) ..) # xn


-- The composition operator 


-- Higher-order library operator .
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f.g = \x -> f (g x)
-- f.g means f composed with g
 
-- Example uses of . operator
odd1 :: Integral a => a -> Bool
odd1 = not . even

twice1 :: (a -> a) -> a -> a
-- twice1 f x = f (f x)
twice1 f = f . f

sumsqreven1 :: [Integer] -> Integer
sumsqreven1 = sum . map (^2) . filter even

-- identity function
id1 :: a -> a
id1 = \x -> x

-- COMPOSE1 IS VERY INTERESTING
-- It takes in a list of functions
-- and then, composes all of those functions
-- and returns back
compose1 :: [a -> a] -> (a -> a)
compose1 = foldr (.) id












