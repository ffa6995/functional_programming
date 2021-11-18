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

t' :: Tree Int
t' = Node (Node (Leaf 1) 3 (Leaf 4))
          5
          (Node (Leaf 6) 7 (Node (Leaf 6) 7 (Leaf 9)))


treeUncomplete :: Tree Int
treeUncomplete = Node (Node (Leaf 1) 3 (Leaf 4))
               5
               (Leaf 6)

isCompleteTree :: Tree a -> Bool
isCompleteTree (Node (Leaf l) x (Node k y m)) = False
isCompleteTree (Node ((Node k y m)) x (Leaf r)) = False
isCompleteTree (Node (Leaf l) x (Leaf r)) = True
isCompleteTree (Node l x r) = (&&) (isCompleteTree l) (isCompleteTree r)

isCompleteTree' :: Tree a -> Bool
isCompleteTree' (Node l x r) = treeDepth l == treeDepth r

-- 7.8.5 Natural Number
-- sub -> test: nat2int (sub (int2nat 8) (int2nat 1))
sub :: Nat -> Nat -> Nat
sub Zero a = Zero
sub a Zero = a
sub (Succ a) (Succ b) = sub a b

-- division -> test: nat2int (division (int2nat 8) (int2nat 4))
division :: Nat -> Nat -> Nat
division Zero a = Zero
division a b = Succ(division (sub a b) b)

-- 7.8.6 Binary Tree
-- countLeaves 
countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node l _ r) = countLeaves l + countLeaves r

-- countNodes
countNodes :: Tree a -> Int
countNodes (Leaf _) = 0
countNodes (Node l _ r) = 1 + countNodes l + countNodes r

-- treeDepth
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 0
treeDepth (Node l _ r) = max (treeDepth l) (treeDepth r) + 1

getValue :: Ord a => Tree a -> a
getValue (Leaf v) = v
getValue (Node l v r) = v

-- isSorted
isSorted :: Ord a => Tree a -> Bool
isSorted (Leaf _) = True
isSorted (Node l x r) = (getValue l < x && getValue r > x) && isSorted l && isSorted r

-- 7.8.7 Key-Value Store
-- TODO: NOT FINISHED
type Key = Int
type Value = String 

data KeyValueStore k v = Empty
                   | Store k v (KeyValueStore k v)
                   deriving Show

keyValueStore :: KeyValueStore Int String
keyValueStore = Store 1 "ABC" (Store 2 "DEF" Empty)

insertToStore :: Eq k => k -> v -> KeyValueStore k v -> KeyValueStore k v
insertToStore k v Empty = Store k v Empty
insertToStore k v (Store ka va kvs) | k == ka = Store ka v kvs
                                    | k /= ka = Store k v (Store ka va kvs)
                                    | otherwise = insertToStore k v kvs

