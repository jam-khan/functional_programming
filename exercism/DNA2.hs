module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

isValid :: [Char] -> Bool
isValid xs = and [x `elem` ['A', 'C', 'G', 'T'] | x <- xs]

getNucleotide :: Char -> Nucleotide
getNucleotide 'A' = A
getNucleotide 'C' = C
getNucleotide 'G' = G
getNucleotide 'T' = T


count :: Nucleotide -> [Char] -> Int
count k xs = length [k | x <- xs, k == getNucleotide x]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs 
  | isValid xs = Right t 
  | otherwise = Left "error"
    where
        t = Data.Map.fromList [(c, count c xs) | c <- [A, C, G, T]]
