import GHC.Conc (numSparks)
import Distribution.Simple.Utils (xargs)

-- product1 [] = 1
-- product1 (x:xs) = x * product1 (xs)

product1 xs = foldr (*) 1 xs



main:: IO()

main = 
    do
        let nums = [1, 2, 3]
            ans = product1 nums
        print ans

