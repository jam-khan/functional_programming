

-- Parser is a program that analyses a piece of text to determine its syntactic structure

data Parser a = P (String -> [(a, String)])

item :: Parser Char
item = P (\inp -> 
            case inp of
                [] -> []
                (x:xs) -> [(x,xs)]
    )

failure :: Parser a 
failure = P (\inp -> [])


