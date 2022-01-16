import System.Random
-- Random Number State

newtype RNGComp a = RNG (StdGen -> (a,StdGen))

toRNGComp :: a -> RNGComp a
toRNGComp a = RNG (\g -> (a, g))

randomRangeRNG :: (Int, Int) -> RNGComp Int 
randomRangeRNG r = RNG (\g -> randomR r g)

(-->) :: RNGComp a -> (a -> RNGComp b) -> RNGComp b
(-->) (RNG fa) cont = RNG (\g -> let (a,g') = fa g
                                     (RNG fb) = cont a
                                 in fb g')

sumOfThreeRolls :: (Int, Int) -> RNGComp Int 
sumOfThreeRolls r = randomRangeRNG r -->
                    \x -> randomRangeRNG r -->
                        \y -> randomRangeRNG r -->
                            \z -> toRNGComp (x+y+z)

runRNGComp :: RNGComp a -> StdGen -> (a,StdGen)
runRNGComp (RNG f) g = f g

evalRNGComp :: RNGComp a -> StdGen -> a 
evalRNGComp rng = fst . runRNGComp rng


-- State
newtype StateComp s a = S (s -> (a,s))

randomRangeState :: (Int, Int) -> StateComp StdGen Int
randomRangeState r = S (\g -> randomR r g)

(--->) :: StateComp s a -> (a -> StateComp s b) -> StateComp s b 
(--->) (S fa) cont = S (\s -> let (a,s') = fa s
                                  (S fb) = cont a
                             in fb s')

toState :: a -> StateComp s a
toState a = S (\s -> (a,s))

sumOfThreeRolls' :: (Int, Int) -> StateComp StdGen Int
sumOfThreeRolls' r = randomRangeState r ---> 
                      \x -> randomRangeState r --->
                          \y -> randomRangeState r --->
                              \z -> toState (x+y+z)

runStateComp :: StateComp s a -> s -> (a,s)
runStateComp (S f) s = f s

evalStateComp :: StateComp s a -> s -> a
evalStateComp rng = fst . runStateComp rng

execStateComp :: StateComp s a -> s -> s
execStateComp rng = snd . runStateComp rng

setState :: s -> StateComp s ()
setState s = S (\_ -> ((),s))

getState :: StateComp s s
getState = S (\s -> (s,s))

modifyState :: (s -> s) -> StateComp s ()
modifyState f = S (\s -> ((), f s))

stateExample :: StateComp Int ()
stateExample = getState ---> 
                \n -> if odd n 
                        then modifyState (*2)
                        else setState 0

