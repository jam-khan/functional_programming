import Text.XHtml (action)

sum1 [] = 0
sum1 (x:xs) = x + sum xs

factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)


average1 xs = sum xs `div` length xs

last1 :: [a] -> a
last1 xs = head (reverse xs)

-- seqn :: [a] -> [a]
-- seqn [] = []
-- seqn (act: acts) = do x <- act
--                       xs <- seqn acts
--                       return (x:xs)

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = ys ++ [x] ++ zs
        where
            ys = [a | a <- xs, a < x]
            zs = [b | b <- xs, b > x]

