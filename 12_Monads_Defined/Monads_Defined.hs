import Data.Foldable
import Control.Monad.State
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

--test liftM2 (+) [0,1] [0,2]
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m m2 = do {x <- m; y <- m2; return (f x y)}

newtype Identity a = Id a deriving Show

instance Functor Identity where
     fmap f (Id a) = Id (f a)

instance Applicative Identity where
     pure a = Id a
     Id f <*> Id a = Id (f a)

instance Monad Identity where
  return a = Id a
  (Id a) >>= f = f a


--mapM
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = f x >>=
    \x' -> mapM' f xs >>=
        \xs' -> return (x' : xs')

--mapM_ return nothing -> void
mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' _ [] = return ()
mapM_' f (x:xs) = f x >>=
   \x' -> mapM_' f xs >>=
     \xs' -> return ()

-- mapM_ with original type
mapM_'' :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_'' g t = mapM_ g $ toList t

--12.12.3 Tree Labeling
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'c') 'b' (Node (Leaf 'e') 'd' (Leaf 'f'))) 'a' (Leaf 'g')

-- pure function
labelTree :: Tree a -> Tree (a, Int)
labelTree (Leaf a) = Leaf (a, 0)
labelTree (Node l a r) = labelTree' (Node l a r) (-1)
      where
        labelTree' :: Tree a -> Int -> Tree (a, Int)
        labelTree' (Leaf a) i = Leaf (a, i + 1)
        labelTree' (Node l a r) i = Node (labelTree' l (i+1))
                                    (a, i+1)
                                    (labelTree' r (treeDepth l + i + 1))
        treeDepth :: Tree a -> Int 
        treeDepth (Leaf _) = 1
        treeDepth (Node l a r) = treeDepth l + treeDepth r + 1

-- With StateComp from Monads Motivated
newtype StateComp s a = S (s -> (a,s))

(-->) :: StateComp s a -> (a -> StateComp s b) -> StateComp s b
(-->) (S fa) cont = S (\s -> let (a,s') = fa s
                                 (S fb) = cont a
                             in fb s')

toState :: a -> StateComp s a
toState a = S (\s -> (a, s))                  

runStateComp :: StateComp s a -> s -> (a,s)
runStateComp (S f) s = f s

evalStateComp :: StateComp s a -> s -> a
evalStateComp rng = fst . runStateComp rng

execStateComp :: StateComp s a -> s -> s
execStateComp rng = snd . runStateComp rng

-- test: evalStateComp (labelTreeState tree) 0
labelTreeState :: Tree a -> StateComp Int (Tree (a, Int))
labelTreeState (Leaf a) = S (\s -> (Leaf (a, s), s + 1))
labelTreeState (Node l a r) = labelTreeState (Leaf a) -->
                               \(Leaf num) -> labelTreeState l -->
                                 \lt -> labelTreeState r -->
                                   \rt -> toState (Node lt num rt)

-- monad
-- test: evalState (labelTreeMonad tree) 0
labelTreeMonad :: Tree a -> State Int (Tree (a, Int))
labelTreeMonad (Leaf a) = do
    i <- get
    put (i+1)
    return (Leaf (a,i))
labelTreeMonad (Node l a r) = do
    num <- labelTreeMonad (Leaf a)
    lt <- labelTreeMonad l
    rt <- labelTreeMonad r
    let (Leaf a) = num
    return (Node lt a rt)
