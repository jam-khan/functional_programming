import Data.Char

type Bit = Int

-- bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * i | (w, i) <- zip weights bits]
--                 where weights = iterate (*2) 1

-- iterate (*2) 1 will produce [1,2,4,8,16]
-- bin2int [a, b, c, d]

-- Math proof sort of

-- 1 * a + 2 * b + 4 * c + 8 * d
-- a + (2 * b) + (4 * c) + (8 * d)
-- a + 2 * (b + 2 * c + 4 * d)
-- a + 2 * (b + 2 * (c + 2 * d))
-- a + (2 * (b + 2 * (c + (2 * (d + 2 * 0)))))

-- foldr (#) v [x0, x1,...,xn] = x0 # (x1 # (... (xn # v) ...))
-- foldl (#) v [x0, x1,...,xn] = (... ((v # x0) # x1) ..) # xn

-- love :: [Bit] -> Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0
-- foldr (\x y -> x + 2 * y) 0
-- I assume y starts from 0 x is coming from values of the list



-- Another method
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
-- Repeat 0 will generate 0 as needed by take 8 due to lazy evaluation of haskell
-- very interesting i must say


