halve :: [a] -> ([a], [a])
halve xs
    | even (length xs) = splitAt (length xs `div` 2) xs
    | otherwise = (xs, [])



third :: [a] -> a
third ns
    | length ns >= 3 = head (tail (tail ns))
    | otherwise = error "Not enough elements"

-- list indexing !!

third1 :: [a] -> a
third1 ns
    | length ns >= 3 = ns !! 2
    | otherwise = error "Not enough elements"

third2 :: [a] -> a
third2 (_:(_:(x:_))) = x
third2 _ = error "Not enough elements"


-- Problem 3

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

safetail1 :: [a] -> [a]
safetail1 ns = if null ns then ns else tail ns

safetail2 :: [a] -> [a]
safetail2 ns
    | null ns = ns
    | otherwise = tail ns

-- Problem 4

(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- Problem 5 and 6

(&&) :: Bool -> Bool -> Bool
a && b = if a then b else False


-- Problem 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z)) 

-- Luhn Algorithm
luhnDouble :: Int -> Int
luhnDouble n 
    | 2 * n <= 9 = 2 * n
    | otherwise = 2 * n - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z k = (luhnDouble x + y + luhnDouble z + k) `mod` 10 == 0
