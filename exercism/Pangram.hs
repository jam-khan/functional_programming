module Pangram (isPangram) where
isPangram :: String -> Bool

isPangram text = and [l `elem` text || u `elem` text | (l, u) <- zip ['a'..'z'] ['A'..'Z']]
