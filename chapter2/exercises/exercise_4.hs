


last1 xs = xs !! (length xs - 1)

last2 xs = head (reverse xs)


init1 ns = take (length ns - 1) ns

init2 ns = reverse (tail (reverse ns))


add1 :: (Int, Int) -> Int
add1 (a, b) = a + b


zeroto :: Int -> [Int]
zeroto n = [0..n]

-- add2 takes in an argument of type int and returns a function
-- that takes in an argument of type int and returns type int
add2 :: Int -> (Int -> Int)
add2 x y = x + y

mult1 :: (Int, Int, Int) -> Int
mult1 (x, y, z) = x * y * z

mult2 :: Int -> (Int -> (Int -> Int))
mult2 x y z = x * y * z

zip1 :: [a] -> [b] -> [(a, b)]
-- zip1 [] bs = []
-- zip1 a [] = []
-- zip1 as bs = [(head as, head bs)] ++ zip1 (tail as) (tail bs)
zip1 xs ys = [(x, y) | x <- xs, y <- ys]

