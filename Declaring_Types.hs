-- Declaring Types

-- 7.8.1 Weekdays
-- Weekday is a type with five data constructors.
data Weekday =
     Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday
   | Sunday

-- 7.8.2 Nat multiplication using recursion and add
data Nat = Zero
      | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

natMult :: Nat -> Nat -> Nat
natMult m Zero = Zero
natMult m (Succ n) = add (natMult m n) m

-- 7.8.3 Implementing folde

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

-- TODO: add String compability
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add z x) = g (folde f g z) (folde f g x)
folde f g (Mul z x) = g (folde f g z) (folde f g x)

-- 7.8.4 Complete Tree
data Tree a = Leaf a
            | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4))
          5
          (Node (Leaf 6) 7 (Leaf 9))

treeUncomplete :: Tree Int
treeUncomplete = Node (Node (Leaf 1) 3 (Leaf 4))
               5
               (Leaf 6)

isCompleteTree :: Tree a -> Bool
isCompleteTree (Node (Leaf l) x (Node k y m)) = False
isCompleteTree (Node (Leaf l) x (Leaf r)) = True
isCompleteTree (Node l x r) = (&&) (isCompleteTree l) (isCompleteTree r)