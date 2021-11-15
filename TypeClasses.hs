-- 9.14.1 Natural Numbers Ord and Enum
data Nat = Zero | Succ Nat

-- Enum Instance
instance Enum Nat where
    toEnum 0 = Zero
    toEnum 1 = Succ Zero
    fromEnum Zero = 0
    fromEnum (Succ Zero) = 1

instance Eq Nat where
    Zero == Zero = True 
    _    == _    = False

-- Ord Instance
instance Ord Nat where
    (<=) d1 d2 = fromEnum d1 == fromEnum d2

-- 9.14.2 Functorial Tree
data Tree a = Leaf a 
            | Branch (Tree a) a (Tree a)
            deriving Show

t :: Tree Int
t = Branch (Branch (Leaf 10) 3 (Leaf 4))
          5
          (Branch (Leaf 6) 7 (Leaf 9))

-- test with: fmap (+1) (Leaf 1)      /   fmap odd t
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

-- 9.14.3 ZipList
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = fmap g (Z xs)
  
instance Applicative ZipList where
  -- pure :: a -> ZipList a
 -- pure x = ...
  
  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  --(Z gs) <$> (Z xs) = ...

-- Monoids
-- 9.14.4.1 Boolean Monids

-- 9.14.4.2 Maybe Monoid

-- 9.14.5 Foldable Tree