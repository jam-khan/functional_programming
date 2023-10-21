-- Exercises
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = f g

sample :: (a -> b) -> (a -> Bool) -> [a] -> [b]
sample f p = map f . filter p

-- Exercise 2
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs)
    | p x = x:Main.takeWhile p xs
    | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs)
    | p x = Main.dropWhile p xs
    | otherwise = x:xs


map1 :: (a -> b) -> [a] -> [b]
map1 p = foldr (\x acc -> p x: acc) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\x acc -> if p x then x:acc else acc) []


dec2int :: [Int] -> Int
dec2int = foldl (\val x -> val * 10 + x) 0 


curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y


pairs :: (a, b) -> a
pairs (x, y) = x
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry 

plus :: Int -> Int -> Int
plus x y = x + y


-- Exercise 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
-- unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Integer
int2bin :: Integer -> [Bit]
-- int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8) 


map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = unfold (\x -> False) f f
-- chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- altMap p1 p2 [] = []
-- altMap p1 p2 (x:xs)
--     | null xs = [p1 x]
--     | otherwise = p1 x : p2 (head xs) : altMap p1 p2 (tail xs)

altMap p1 p2 = foldl (\acc x -> if even (length acc) then acc ++ [p1 x] else acc ++ [p2 x]) []


