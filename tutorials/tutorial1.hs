-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry f x y = f (x, y)

-- uncurried :: (a -> b -> c) -> (a, b) -> c
-- uncurried f (x, y) = f x y 


-- insert :: Int -> [Int] -> [Int]
-- insert x [] = [x]
-- insert x (y:ys)
--     | x <= y = x : y : ys 
--     | otherwise = y: insert x ys 


-- -- sample quickcheck application
-- prop_MaxLe :: Int -> Int -> Property
-- prop_MaxLe x y =
--     x <= y ==> max x y == y 


data Expr = Val Int | Add Expr Expr | Mult Expr Expr


eval :: Expr -> Int 
eval (Val n) = n 
eval (Add x y) = eval x + eval y 
eval (Mult x y) = eval x * eval y 


fold :: (Int -> a) -> (a-> a -> a) -> (a -> a -> a) -> Expr -> a
fold f lf rf (Val n) = f n
fold f lf rf (Add x y) = lf (fold f lf rf x) (fold f lf rf y)
fold f lf rf (Mult x y) = rf (fold f lf rf x) (fold f lf rf y)

