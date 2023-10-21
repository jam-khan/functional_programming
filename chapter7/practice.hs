-- Higher Order Functions

twice1 :: (a -> a) -> a -> a
twice1 f x = f (f x)


-- What is a higher order function?

-- A function that takes a function as an argument
-- or returns a function as a result
-- is called a higher-order function

-- Processing lists

-- map :: (a -> b) -> [a] -> [b]
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
-- filter f xs = [x | x <- xs, f x]


filter1 _ [] = []
filter1 p (x:xs)
    | p x = x: filter1 p xs
    | otherwise = filter1 p xs

sumsqreven :: Integral a => [a] -> a
sumsqreven ns = sum (map (^2) (filter even ns))

-- Important functions
-- all p xs
-- any p xs
-- takeWhile p xs
-- dropWhile p xs

sum1 :: Num a => [a] -> a
sum1 = foldr (+) 0

product1 :: Num a => [a] -> a
product1 = foldr (*) 1

or :: [Bool] -> Bool 
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

-- Above is an example of arguments made implicitly


length1 :: [a] -> Int
length1 = foldr (\_ n -> n + 1) 0 

length2 :: [a] -> Int
length2 = foldl (\n _ -> n + 1) 0

-- Above marks an important difference between foldl and foldr


reverse2 :: [a] -> [a]
reverse2 = foldr (\a b -> b ++ [a]) []


sum2 :: Num a => [a] -> a
sum2 = sum1' 0
    where
        sum1' v [] = v
        sum1' v (x:xs) = sum1' (v + x) xs


-- Right associative recursive function example
-- f [] = v
-- f (x:xs) = x # f xs

-- Left associative recursive function example
-- f v [] = v
-- f v (x:xs) = f (v # x) xs

foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f v [] = v
foldl2 f v (x:xs) = foldl2 f (f v x) xs


foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

-- Composition operator

(.) :: (b -> c) -> (a -> b) -> (a -> c)

f . g = \x -> f (g x)