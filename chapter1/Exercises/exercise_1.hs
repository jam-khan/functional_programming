import Distribution.Simple.Utils (xargs)
import Language.Haskell.TH (valD)


double :: (Num a) => a -> a

double x = x + x



main :: IO()

main = 
    do
        let ans = double 2

        print ans


-- Method 1 to do the break down double
-- double 6
-- 3 + 3
-- 6

-- double (double 2)
-- method 1: double 2 + double 2 = 4 + 4 = 8

-- method 2: double (2 + 2) = double 4 = 4 + 4 = 8

-- method 3: double (double 2) = double (2 + 2) = (2 + 2) + (2 + 2) = 4 + (2 + 2) = 4 + 4 = 8
-- method 4: double (double 2) = (double 2) + double 2 = (2 + 2) + double 2 = 4 + double 2 = 4 + (2 + 2) = 4 + 4 = 8


