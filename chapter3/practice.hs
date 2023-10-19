
add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]


-- Functions do not have to be total on their argument type
-- in the sense that there may be some arguments for which the result
-- is not defined.

-- Curried functions

add' :: Int -> (Int -> Int)
add' x y = x + y
-- This definition is weird
-- add' does not takes 2 arguments
-- it takes 1 argument x
-- then, it returns another function
-- that takes argument y which in turn return x + y

-- Functions taking more than 2 arguments
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- Description
-- mult takes in argument x
-- return another
-- which takes argument y
-- returns another function
-- which takes argument z
-- returns x * y * z
-- finally, result is done


-- Functions which take arguments one at a time
-- are called curried functions

-- Curried functions are more useful. How?
-- Useful functions can often be made by partially applying a curried function
-- with less than its full complement of arguments.

-- E.g. add' 1 becomes an incrementer function
-- Test it with fmap (add' 1) [1,2,3,4]
-- Int -> Int -> Int -> Int means Int -> (Int -> (Int -> Int))


-- What is Right-Associative?
-- In the absence of parenthesis
-- operations are grouped from right
-- Example: Int -> Int -> Int -> Int means (Int -> (Int -> (Int -> Int)))
-- Example: Exponentiation 2 ^ 3 ^ 4 means 2 ^ (3 ^ 4)
-- What is Left-Associative?
-- In the absence of parenthesis
-- operations are grouped from left
-- Example: Function application with spacing
-- mult x y z means ((mult x) y) z



-- Operations that are left-associative

-- Arithmetic Operators: +, -, /, *
-- Comparison Operators: <, >, <=, >=, 
-- List Concatenation: list1 ++ list2 ++ list3 means (list1 ++ list2) ++ list3

-- Operations that are right-associative

-- Exponentiation: 2 ^ (3 ^ 2)
-- Function Composition: f . (g . h)
-- Right List Cons: 1 : 2 : 3 : [] equals 1 : (2 : (3: []))
-- Function Application: mult x y z equals ((mult x) y) z

-- All functions in Haskell with multiple arguments are normally defined as curried functions

-- Polymorphic types
-- 


-- type variables are lower-case and usually a, b, c, and so on
-- length :: [a] -> Int

love:: [a] -> Int
love [] = 0
love (x:xs) = 1 + love xs

-- A type that contains one or more type variables is called
-- polymorphic ("of many forms")

-- Overloaded types

-- Class Constraints
-- example: (+) :: Num a => a -> a -> a

-- A class is a collection of types that support
-- certain overloaded operations called methods

-- Eq - equality types
-- class contains types whose values can be compared
-- for equality and inequality using following
-- two methods: (==) and (/=)

-- All the basic types 
-- Bool, Char, String, Int, Integer, Float, and Double
-- are instances of the Eq class, as are list and tuple types, 
-- provided that their element and component types are instances

-- Ord - ordered types (contains types that are instance of Eq class + contains types that are totally (linearly) ordered)
-- Methods:
--      (<)  :: Ord a => a -> a -> Bool
--      (<=) :: Ord a => a -> a -> Bool
--      (>)  :: Ord a => a -> a -> Bool
--      (>=) :: Ord a => a -> a -> Bool
--      (min):: Ord a => a -> a -> Bool
--      (max):: Ord a => a -> a -> Bool
-- Interesting: False < True

-- Types that are instances of
    -- Bool
    -- Char
    -- String
    -- Int 
    -- Integer
    -- Float
    -- Double

-- Show -- showable types
-- this class contains type values that can be converted into strings
-- of characters using the following method

-- show :: a -> String

-- Following Basic Instances are
    -- Bool
    -- Char
    -- String
    -- Int
    -- Integer
    -- Float
    -- Double

-- Read - readable types
    -- read :: String -> a
    -- This class is dual to Show
    -- Bool, Char, String, Int, Integer, Float, and Double are instances of the Read class

