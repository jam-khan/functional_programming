main = do
    -- In Haskell, function is a mapping. 
    -- It takes some arguments and returns a single result.
    -- e.g.
    -- double x = x + x
    -- solving double(double (2))
    -- Two ways to do it.
    -- Call by value: double (2 + 2)
    -- or, Call by name: double 2 + double 2

    -- imperative languages are the ones where basic method of computation is changing stored values
    -- examples:
    -- total = 0
    -- for i in range(len(5)):
    --     total += 1

    -- vs.
    -- sum [1..5] => sum[1,2,3,4,5] => 1+2+3+4+5 = 15

    -- double x = x + x
    -- sum function can be defined using following two equations:
    -- sum [] = 0
    -- sum (n:ns) = n + sum ns e.g. if n:ns = [1,2,3], then n = 1 and ns = [2, 3]
    