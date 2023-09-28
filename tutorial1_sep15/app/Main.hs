module Main where
import GHC.Float (int2Double)
import Distribution.Simple.Utils (xargs)

main :: IO ()
main = putStrLn "Hello, Haskell!"


-- >>> sum[0..5]
-- 15


sum123 :: Int
sum123 = foldr (+) 0 [1, 2, 3,4]

-- >>> sum123
-- 



last1 :: [int] -> int

-- last1 xs = xs !! (length xs - 1)

-- approach 2

last1 xs = head (reverse xs) 



-- method 2
last2 :: [a] -> a
last2 [] = error "calling last on empty list"
last2(x : []) = x
last2 (x:y:xs) = last(y: xs)



-- init

init4 :: [a] -> [a]

init4 xs = take ( (length xs) - 1) xs 

init5 :: [a] -> [a]
init5 [] = error "Calling init on empty"
init5 (x: []) = []
init5 (x: y: xs) = x : init (y: xs)



