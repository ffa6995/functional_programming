-- 9.14.1 Natural Numbers Ord and Enum
data Nat = Zero | Succ Nat

-- Enum Instance
instance Enum Nat where
  toEnum 0 = Zero
  toEnum x = Succ (toEnum (x - 1))
  fromEnum Zero = 0
  fromEnum (Succ x) = fromEnum x + 1

instance Eq Nat where
  Zero == Zero = True
  Succ a == Succ b = a == b
  _ == _ = False

-- Ord Instance
instance Ord Nat where
  Zero <= Zero = True
  Zero <= Succ x = True
  Succ x <= Zero = False
  Succ x <= Succ y = x <= y

-- 9.14.2 Functorial Tree
data Tree a
  = Leaf a
  | Branch (Tree a) a (Tree a)
  deriving (Show)

t :: Tree Int
t =
  Branch
    (Branch (Leaf 10) 3 (Leaf 4))
    5
    (Branch (Leaf 6) 7 (Leaf 9))

-- test with: fmap (+1) (Leaf 1)      /   fmap odd t
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

-- 9.14.3 ZipList
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  --fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  --pure :: a -> ZipList a
  pure x = Z (repeat x)

  --(<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z (zipWith (\f x -> f x) gs xs)

-- Monoids
-- 9.14.4.1 Boolean Monids
newtype BooleanAnd = BA Bool deriving Show
newtype BooleanOr =  BO Bool deriving Show

instance Semigroup BooleanAnd where 
  (<>) (BA x) (BA y) = BA (x && y)

instance Monoid BooleanAnd where
  mempty = BA False
  mappend = (<>)

instance Semigroup BooleanOr where 
  (<>) (BO x) (BO y) = BO (x || y)

instance Monoid BooleanOr where
  mempty = BO False
  mappend = (<>)


-- 9.14.4.2 Maybe Monoid
newtype AddMaybe a = AddMaybe (Maybe a) deriving Show

instance Num a => Semigroup (AddMaybe a) where
    (<>) (AddMaybe (Just a)) (AddMaybe (Just b)) = AddMaybe (Just (a + b))
    (<>) (AddMaybe (Just a)) (AddMaybe Nothing) = AddMaybe (Just a)
    (<>) (AddMaybe Nothing) (AddMaybe (Just a)) = AddMaybe (Just a)
    (<>) (AddMaybe Nothing) (AddMaybe Nothing) = AddMaybe Nothing

instance Num a => Monoid (AddMaybe a) where
    mempty = AddMaybe Nothing 
    mappend = (<>)

-- 9.14.5 Foldable Tree

-- treeDepth

instance Foldable Tree where
  foldr f g (Leaf _) = g
  foldr f g (Branch l x r) = foldr f (f x (foldr f g r)) l

  foldMap f (Leaf _) = mempty
  foldMap f (Branch l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

-- functions which need to distinguish between elements of the
-- structure (e.g. in a Tree) are impossible to implement using only Foldable.
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 0
treeDepth (Branch l _ r) = max (treeDepth l) (treeDepth r) + 1