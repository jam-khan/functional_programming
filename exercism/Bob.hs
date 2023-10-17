module Bob (responseFor) where

question :: [Char] -> Bool
question "" = False
question xs = last xs == '?'

yell :: [Char] -> Bool
yell "" = False
yell text = (and [x `elem` ['A'..'Z'] || (x `notElem` ['a'..'z']) | x <- text]) && not (null [k | k <- text, k `elem` ['A'..'Z']])

responseFor :: String -> String
responseFor text
    | yell xs && question xs = "Calm down, I know what I'm doing!"
    | yell xs = "Whoa, chill out!"
    | question xs = "Sure."
    | xs == "" = "Fine. Be that way!"
    | otherwise = "Whatever."
      where
        xs = [k | k <- text, k `notElem` ['\t', '\n', '\r', ' ']]

