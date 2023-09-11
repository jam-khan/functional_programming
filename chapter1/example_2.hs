-- This is an example of generic function, which is capable of taking 
-- a list of input/output action, such as reading or writing, and executes
-- and returns a list of these values

seqn [] = return []
seqn (act:acts) = do 
                    x <- act
                    xs <- seqn acts
                    return (x:xs)

main :: IO ()

main = do
    let sampleList = [getChar, getChar]
    
    ans <- seqn sampleList
    
    putStrLn "Results: "
    -- mapM_ putStrLn ans
    print ans
