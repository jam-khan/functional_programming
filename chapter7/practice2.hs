

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

processData :: [Int] -> [Int]
processData = compose [reverse, filter (>5)]


-- fun :: [Int] -> Bool
-- fun = and . map even

-- -- Today, first time in my life
-- -- I will just try to maximize my focus time.

-- -- Composition is associative
-- -- f . g . h = (f . g) . h = f . (g . h)
-- id1 :: a -> a
-- id1 = \x -> x

-- id . f = f
-- f . id = f for any function f

