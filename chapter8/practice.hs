
-- Examples of type declarations

type Pos = (Int, Int)
type Trans = Pos -> Pos -- It can take function types


-- Not allowed: type Tree = (Int, [Tree])

type Pair a = (a, a)
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']


-- Data declarations
-- data Bool = False | True
-- Constructors
-- New possible values of the type are called
-- Constructors.
-- In this case, these are False and True

-- Names given to new types
-- Name of new type must begin with a capital letter
-- Same constructor name cannot be used in
-- more than one type.

data Move = North | South | West | East deriving Show 

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move West (x, y) = (x - 1, y)
move East (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p 
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move 
rev North = South 
rev South = North 
rev West = East 
rev East = West 

data Shape = Circle Float | Rect Float Float 

square :: Float -> Shape 
square n = Rect n n

area :: Shape -> Float 
area (Circle r) = pi * r ^ 2
area (Rect w h) = w * h 

-- With the use of arguments,
-- Constructors become constructor functions

data Maybe a = Nothing | Just a 
             deriving (Show, Eq, Ord, Read)


data Maybe1 a = Nothing1 | Just1 a 
             deriving (Show, Eq, Ord, Read)

safediv :: Int -> Int -> Main.Maybe Int
safediv _ 0 = Main.Nothing 
safediv x y = Main.Just (x `div` y)

safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing 
safehead (x:xs) = Main.Just x

-- newtype Nat = N Int 
-- type Na
-- newtype Nat = N Int 
data Nat = Zero | Succ Nat deriving Show 

nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat 
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)


data List a = Nil | Cons a (List a)

len :: List a -> Int 
len Nil = 0
len (Cons _ xs) = 1 + len xs


-- Suitable type for Tree
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t:: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf n) = n == x 
-- occurs x (Node lt n rt) = n == x || occurs x lt || occurs x rt


-- flattens
flatten :: Tree a -> [a]
flatten (Leaf n) = [n]
flatten (Node l m r) = flatten l ++ [m] ++ flatten r 


-- We will do an improved version of occurs
-- with better time complexity using BST properties

occurs :: Ord a => a -> Tree a -> Bool 
occurs x (Leaf y)                   = x == y 
occurs x (Node l m r) | x == m      = True
                      | x < m       = occurs x l
                      | otherwise   = occurs x r 

-- Multiple Tree forms

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)
data Tree2 a = Leaf2 a | Node2 (Tree2 a) a (Tree2 a)
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)

-- Below allows for multiple childs
data Tree4 a = Node4 a [Tree4 a]



-- Section 8.5 
-- Class and Instance declarations
-- class Eq a where
--     (==), (/=) :: a -> a -> Bool 

--     x /= y = not (x == y)

-- class Eq a where
--     (===), (/==) :: a -> a -> Bool

--     x /== y = not (x === y)

-- instance Main.Eq Bool where
--     False === False = True
--     True === True = True
--     _ === _ = False 


-- class Main.Eq a => Ord a where
--     (<<<), (<<<=), (>>>), (>>>=) :: a -> a -> Bool 
--     min, max             :: a -> a -> a 

--     min x y | x <<<= y    = x
--             | otherwise = y
    
--     max x y | x >>>= y    = x
--             | otherwise = y

-- instance Main.Ord Bool where 

--     False <<< True = True
--     _ <<< _ = False 

--     b <<<= c = (b <<< c) || b === c 
--     b >>> c = c <<< b 
--     b >>>= c = c <<<= b


data Fun = F | T
           deriving (Prelude.Eq, Prelude.Ord, Show, Read)





-- Tautology Checker

data Prop = 
    Const Bool      |
    Var Char        |
    Not Prop        |
    And Prop Prop   |
    Or Prop Prop    | 
    Imply Prop Prop 
    deriving Show
-- why is there no or
-- Explicit constructor for parenthesis is not required
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop 
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop 
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop 
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Associating a variable with corresponding Bool value
type Subst = Assoc Char Bool 

sample :: Subst 
sample = [('A', False), ('B', True)]

eval :: Subst -> Prop -> Bool 
eval _ (Const b)    = b
eval s (Var b)      = find b s
eval s (Not b)      = not (eval s b)
eval s (And a b)    = eval s a && eval s b
eval s (Imply p q)  = eval s p <= eval s q
eval s (Or p q)     = eval s p || eval s q


vars :: Prop -> [Char]
vars (Const _) = []
vars (Var b)   = [b]
vars (Not b)   = vars b
vars (And a b) = vars a ++ vars b
vars (Imply a b) = vars a ++ vars b 
vars (Or a b) = vars a ++ vars b


bools :: Int -> [[Bool]]
bools 1 = [[False], [True]]
bools n = [x:y | x <- [False, True], y <- ys]
        where
            ys = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) 
    | null [k | k <- xs, k == x] = x: rmdups xs
    | otherwise = rmdups xs 

substs :: Prop -> [Subst]
substs p = [zip vs b | b <- bs]
    where 
        vs = rmdups (vars p)
        bs = bools (length vs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]