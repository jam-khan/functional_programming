import Distribution.Simple.Utils (xargs)
-- Mistake 1: Name should not start with capital

simple = a `div` length xs
        where
            a = 10
            xs = [1, 2, 3, 4 , 5]
