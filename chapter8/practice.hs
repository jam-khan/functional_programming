-- Type declarations in Haskell can not be recursive

-- This is not allowed: type Tree = (Int, [Tree])

type Pair a = (a, a);

type Assoc k v = [(k, v)]

test :: a -> Pair a
test k = (k, k)


find1 :: Eq k => k -> Assoc k v -> v
find1 k t = head [v | (k', v) <- t, k' == k]

data Move = North | South | East | West deriving Show
type Pos = (Int, Int)
move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y) 

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (x:xs) p = moves xs (move x p)


data Shape = Circle Float | Rect Float Float


square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect b h) = b * h


-- Constructor function vs normal functions
-- Constructor functions do not typically process the data
-- They exist purely for the sake of defining data

data Maybe1 a = Nothing1 | Just1 a deriving Show

-- Data declarations can also be parametrized
safediv :: Int -> Int -> Maybe1 Int
safediv _ 0 = Nothing1
safediv m n = Just1 (m `div` n)


-- Newtype declarations

-- type Nat = Int
-- data Nat = N Int
-- newtype Nat = N Int


-- new types can be declared using data and newtype mechanisms
-- these type declarations can be recursive in nature.
-- newtype Nat = N Int
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))


add1 :: Nat -> Nat -> Nat
add1 Zero n = n
add1 (Succ m) n = Succ (add1 m n)


data List1 a = Nil | Cons a (List1 a)
len1 :: List1 a -> Int
len1 Nil = 0
len1 (Cons _ xs) = 1 + len1 xs
-- Cons x xs

-- Tree Node type

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = (occurs x l) || y == x || (occurs x r) -- Inorder traversal

-- inorder traversal
flatten1 :: Tree a -> [a]
flatten1 (Leaf m) = [m]
flatten1 (Node l m r) = flatten1 (l) ++ [m] ++ flatten1 (r)

-- preorder
flatten2 :: Tree a -> [a]
flatten2 (Leaf m) = [m]
flatten2 (Node l m r) = m:(flatten2 l ++ flatten2 r)

flatten3 :: Tree a -> [a]
flatten3 (Leaf m) = [m]
flatten3 (Node l m r) = (flatten3 l ++ flatten3 r) ++ [m]


-- If we know that the tree is a search tree
occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y) = x == y
occurs2 x (Node l m r)
    | x == m = True
    | x < m = occurs2 x l
    | otherwise = occurs2 x r


mydivision :: Int -> Int -> Int
mydivision x y =
        case safediv x y of
            Nothing1 -> 0
            Just1 n -> n + 1


-- Class and instance declarations

-- class Eq a where
--     (==), (/=) :: a -> a -> Bool

--     x /= y = not (x == y)



occurs1 :: Ord a => a -> Tree a -> Bool
occurs1 x (Leaf a) = a == x
occurs1 x (Node l m r) 
    |   x == m = True
    |   occurs x l = True
    |   occurs x r = True
    |   otherwise = False


occurs3 x (Node l m r ) = (x == m) || occurs3 x l || occurs3 x r


flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l m r) = flatten l ++ [m] ++ flatten r


occurs4 :: Ord a => a -> Tree a -> Bool
occurs4 x (Leaf i) = x == i
occurs4 x (Node l m r)
    | x == m = True
    | x < m = occurs2 x l
    | otherwise = occurs2 x r
