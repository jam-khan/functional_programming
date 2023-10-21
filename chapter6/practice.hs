-- Recursive functions

(*) :: Int -> Int -> Int
m * 0 = 0
m * n = m + (m Main.* (n - 1))


reverse1 :: [a] -> [a]
reverse1 [] = []
-- reverse1 (x:xs) = reverse1 xs ++ [x]

-- Interesting

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x:xs) ++ rs = x :(xs Main.++ rs)


insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
insert1 x (y:ys) | x <= y = x: y: ys
    | otherwise = y: insert1 x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert1 x (isort xs)

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x, y): zip1 xs ys

-- Mutual Recursion Example

even1 :: Int -> Bool
even1 0 = True
even1 n = odd1 (n - 1)

odd1 :: Int -> Bool
odd1 0 = False
odd1 n = even1 (n - 1)


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:(odds xs)

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = evens xs


-- Drop definition

drop1 :: Integral b => b -> [a] -> [a]
drop1 0 xs = xs
drop1 _ [] = [] 
drop1 n (x:xs) = drop1 (n - 1) xs


init1 :: [a] -> [a]
init1 [_] = []
init1 (x:xs) = x:init1 xs
-- init1 (x:xs) | null xs = []
--              | otherwise = x:init xs
