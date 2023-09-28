
factors1 ::Int -> [Int]
factors1 n = [x | x <- [1..n], n `mod` x == 0]


primes :: Int -> Bool
primes n = factors1 n == [1,n]


zip1 :: [a] -> [b] -> [(a,b)]
zip1 (x:xs) (y:ys) = [(x, y)] ++ zip1 xs ys
zip1 _ _ = []


count1 :: Int -> [Int] -> Int
count1 x xs =  head( reverse [i | x' <- xs, x' == x, i <- [0..]])

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f (x:xs)
    | f x = x:filter1 f xs
    | otherwise = filter1 f xs
filter1 f [] = []


reverse1:: [a] -> [a]
reverse1 = foldr (\x xs -> xs ++ [x]) []