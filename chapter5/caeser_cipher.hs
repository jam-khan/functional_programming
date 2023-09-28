-- using standard function on characters
import Data.Char

-- lower case to int
-- let2int :: Char -> Int
-- let2int c 
--     | isAsciiLower c = (ord c `mod`  26) + ord 'a'
--     | isAsciiUpper c = (ord c `mod` 26) + ord 'A'
--     | otherwise = ord c

-- int2let :: Int -> Char
-- int2let x 
--     | x >= ord 'a' = chr (((x - ord 'a') `mod` 26) + ord 'a')
--     | otherwise = chr (((x - ord 'A') `mod` 26) + ord 'A')

-- -- Defining isLower our version
-- -- isLower1 :: Char -> Bool
-- -- isLower1 ch = ch >= 'a' && ch <= 'z'
-- -- -- Another way to do it
-- -- isLower2 :: Char -> Bool
-- -- isLower2 ch = isAsciiLower ch
-- -- or simply use it directly


-- shift :: Int -> Char -> Char
-- shift n c
--     | isAsciiLower c || isAsciiUpper c = int2let (ord c + (n `mod` 26))
--     | otherwise = c


-- encode1 :: Int -> String -> String
-- encode1 n xs = [shift n x | x <- xs]

let2int :: Char -> Int
let2int ch = ord ch - ord 'a'

int2let :: Int -> Char
int2let i = chr (i + ord 'a')


shift :: Int -> Char -> Char
shift n x 
    | x >= 'a' && x <= 'z' = int2let ((let2int(x) + n) `mod` 26)
    | otherwise = x

encode1 :: Int -> String -> String
encode1 n xs = [shift n x | x <- xs]
