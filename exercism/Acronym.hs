module Acronym (abbreviate) where
import Data.Char (ord)
import Data.Char (chr)
import Data.Char (toUpper)

removePunc :: String -> String
removePunc (x:xs) 
  | null xs = ""
  | x `elem` ['A'..'Z']++['a'..'z']++['-',' '] = x:removePunc xs
  | otherwise = removePunc xs


wordSplit :: [Char] -> [Char] -> [[Char]]
wordSplit [] [] = [] 
wordSplit (x:xs) [] = wordSplit xs [x]
wordSplit [] word = [word]
wordSplit (x:xs) word
  | x `elem` [' ', '-'] = word: wordSplit xs []
  | otherwise = wordSplit xs (word ++ [x])

findCapital :: String -> [Char]
findCapital [] = []
findCapital (x:xs)
  | x `elem` ['A'..'Z'] && (not (null xs) && head xs `notElem` ['A'..'Z']) = toUpper (x):findCapital xs
  | otherwise = findCapital xs

getOrNot :: Char -> String
getOrNot word 
    | word `elem` [' ', '-'] = []
    | otherwise = [word]

abbreviate:: String -> String
abbreviate xs = concat [getOrNot (toUpper (head word)) ++ findCapital (tail word) | word <- wordSplit (removePunc xs) []]
              