
-- 11:25 PM 04th Nov 2023


inc :: [Int] -> [Int]
inc []      = []
inc (n: ns) = n + 1: inc ns 

sqr :: [Int] -> [Int]
sqr []      = []
sqr (n:ns)  = n ^ 2 : sqr ns


map1 :: (a -> b) -> [a] -> [b]
map1 f []        = []
map1 f (x:xs)    = f x : map1 f xs



-- What are functors?
-- In functional programming, a functor is a type that implements
-- map function which given a function 'f' and a functor 'F', applies 
-- the 'f' to the contents of 'F'


-- Requirements to be a functor
-- 1. Identity:
--      If you map a identity function over a functor, the functor that
--      you get back should be the same as the original functor
-- 2. Composition:
--      Functors must preserve composition of functions.
--      That is if I have two functions f and g
--      then, mapping 'f' after 'g' should be the same as mapping their composition


class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b 

-- Above means that for a paramterized type f to be an instance of the class Functor, it must support
-- a function fmap of the specified type


-- Explanation: fmap takes a function of type (a -> b) and applies it to 
-- the elements of the parametrized type f a to give a structure of type f b

-- instance Functor1 [] where 
--     fmap :: (a -> b) -> [a] -> [b]
--     fmap = map


-- data Maybe a = Nothing | Just a 

-- We are going to make the maybe type into a functor
-- functor is the type that implements map function
instance Functor1 Maybe where 
    fmap1 :: (a -> b) -> Maybe a -> Maybe b
    fmap1 _ Nothing      = Nothing
    fmap1 g (Just x)     = Just (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- Let's make a functor of Tree
instance Functor1 Tree where 
    fmap1 :: (a -> b) -> Tree a -> Tree b
    fmap1 g (Leaf x)     = Leaf (g x)
    fmap1 g (Node l r)   = Node (fmap1 g l) (fmap1 g r)


-- In a paramterized type data structure f a
-- type a is, sometimes, called a container type

instance Functor1 IO where 
    fmap1 :: (a -> b) -> IO a -> IO b 
    fmap1 g mx = do {x <- mx; return (g x)}

