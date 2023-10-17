module DNA (toRNA) where

getComplement :: Char -> Char
getComplement x
  | x == 'G' = 'C'
  | x == 'C' = 'G'
  | x == 'T' = 'A'
  | x == 'A' = 'U'
  | otherwise = x


findValid :: [Char] -> Bool
findValid xs = and [getComplement x /= x | x <- xs]

toRNA :: String -> Either Char String
toRNA xs 
  | findValid xs = Right [getComplement x | x <- xs]
  | otherwise = Left t
    where
      t = head [x | x <- xs, getComplement x == x]