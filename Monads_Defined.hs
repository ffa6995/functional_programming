-- 12. Monads Defined

--12.12.1 CPS quadratic equation
quadEq :: (Float, Float, Float) -> (Float, Float)
quadEq (a, b, c) = (x1, x2)
    where
        x1 = e + sqrt d / (2 * a)
        x2 = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)

calcD :: (Float, Float, Float) -> (Float -> r) -> r
calcD (a, b, c) k = k (b * b - 4 * a * c)

calcE :: (Float, Float) -> (Float -> r) -> r
calcE (a, b) k = k (- b / (2 * a))

quadtraticEq :: (Float, Float, Float) -> Float -> Float -> ((Float, Float) -> r) -> r
quadtraticEq (x, y, z) d e k = k (e + sqrt d / (2 * x), e - sqrt d / (2 * x))

-- Beispiel quadraticCps (1,5,6) print
-- Ergebnis = (-2.0,-3.0)
quadraticCps :: (Float, Float, Float) -> ((Float, Float) -> r) -> r
quadraticCps (x, y, z) k =
  calcD (x, y, z) $ \d ->
  calcE (x, y) $ \e ->
  quadtraticEq (x, y, z) d e k
  
--12.12.2 Monad Basics
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \a -> return (f a)

newtype Identity a = Id { runIdentity :: a }

instance Functor Identity where
     fmap f (Id x) = Id (f x)

instance Applicative Identity where
     pure = Id
     Id f <*> Id x = Id (f x)

--test liftM2 (+) [0,1] [0,2]
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m m2 = do {x <- m; y <- m2; return (f x y)}

--mapM
--mapM :: Monad m => (a -> m b) -> [a] -> m [b]

--mapM_ return nothing -> void
--mapM_ mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

-- mapM_ with original type
--mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

--12.12.3 Tree Labeling



data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

