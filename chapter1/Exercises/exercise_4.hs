quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []        -- Base case: an empty list is already sorted
quicksort (x:xs) =              -- Recursive case:
    quicksort larger ++ [x] ++ quicksort smaller
    where
        smaller = [a | a <- xs, a > x]
        larger = [b | b <- xs, b < x]
        

main :: IO ()
main = do
    let unsortedList = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
        sortedList = quicksort unsortedList
    putStrLn "Unsorted List: "
    print unsortedList
    putStrLn "Sorted List: "
    print sortedList