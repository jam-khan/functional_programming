-- Declaring types and classes

type String = [Char]
type Pos = (Int, Int)

type Trans = Pos -> Pos


-- Note: type declarations can not be recursive

-- type Tree = (Int, [Tree]) NOT ALLOWED

type Pair a = (a, a)

type Assoc k v = [(k, v)]

find1 :: Eq k => k -> Assoc k v -> v
find1 k t = head [v | (k', v) <- t, k == k']

-- Data declarations

-- Prelude declaration
-- data Bool = False | True
-- constructor symbol | is read as or

data Move = North | South | East | West deriving Show


move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x - 1, y)
move West (x, y) = (x + 1, y)


moves :: Pos -> [Move] -> Pos
moves p = foldr (\mv acc -> move mv acc) p 


rev :: Move -> Move
rev North = South 
rev South = North 
rev East = West
rev West = East


data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n 

area :: Shape -> Float 
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y 

-- Expression Circle 1.0 is already fully evaluated

data Maybe a = Nothing | Just a deriving Show 

safediv1 :: Int -> Int -> Main.Maybe Int
safediv1 _ 0 = Main.Nothing
safediv1 m n = Main.Just (m `div` n)

safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing
safehead xs = Main.Just (head xs)


-- type Nat = Int
-- data Nat = N Int



-- Data introduces run-time overhead when used,
-- as it has to store information about which constructor was used.
-- 
-- newtype provides better type safety
-- defines new type based on existing type
-- It can only have 1 constructor
-- provides type safety without runtime overhead
-- runtime representation of newtype is 
-- identical to its underlying type

-- Recursive types

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

