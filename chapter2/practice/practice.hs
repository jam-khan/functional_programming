import Distribution.Simple.Utils (xargs)
-- How to write a function in haskell

-- Example: f(a, b) + cd


-- Haskell: f a b + c* d

{-
    Examples
        f(x) == f x
        f(x, y) == f x y
        f(g(x)) == f (g x)
        f(x, g(y)) == f x (g y)
        f(x)g(y) == (f x) * (g y) can also write it as f x * g y
-}

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)
        
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

average1 :: Foldable t => t Int -> Int
average1 xs = sum xs `div` length xs


