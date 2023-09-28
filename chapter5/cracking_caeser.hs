
import Data.Char
-- frequency table of alphabets in english words
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


percent1 :: Int -> Int -> Float
percent1 x y = (fromIntegral x / fromIntegral y) * 100


-- Library function
-- fromIntegral will convert a floating point integer
count1 :: Char -> String -> Int
count1 ch xs = length [1 | k <- xs, k == ch]

-- If we had called lowers xs multiple times, it will be slow
-- n = lowers xs calculates once
-- rotatedChi :: [Float] -> [Float]
-- rotatedChi freqtable = [chisqr (rotate n freqtable) table | n <- [0..25]]


lowers :: String -> Int
lowers xs = length [1| x <- xs, x >= 'a' && x <= 'z']

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

positions2 :: Float -> [Float] -> [Int]
positions2 x xs = [i | (c, i) <- zip xs [0..], x == c]

freqs :: String -> [Float]
freqs xs = [percent1 (count1 x xs) n| x <- ['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


crack :: String -> String
crack xs = encode1 (-factor) xs
    where
        factor = head (positions2 (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs