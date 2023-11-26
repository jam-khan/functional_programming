f :: Int -> (Int -> (Int -> (Int -> Int)))
f x y z k = x + y + z + k 

class Printable1 a where
    display :: a -> String

class Describable1 a where
    describe :: a -> String


instance Printable1 Integer where
    -- display :: Int -> String
    display x = "This is an integer: " ++ show x

instance Describable1 Integer where
    -- describe :: Int -> String
    describe x = "It has a value of " ++ show x

info :: (Printable1 a, Describable1 a) => a -> String
info x = display x ++ ". " ++ describe x

(&&&) :: Bool -> Bool -> Bool 
a &&& b = if a then b else False

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum [y | y <- [1..x-1], x `mod` y == 0] == x]