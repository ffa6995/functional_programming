-- Exercise 10 - Random
-- command: "stack ghci ./Random.hs"

import System.Random

-- 10.5.1 sumRand
sumOfThreeRolls :: RandomGen g => (Int, Int) -> g -> (Int, g)
sumOfThreeRolls r g = (r1+r2+r3, g2)
    where
        (r1, g0) = randomR r g
        (r2, g1) = randomR r g0
        (r3, g2) = randomR r g1

-- test: sumRand (1,6) 1 (mkStdGen 42) sumOfThreeRolls
sumRand :: RandomGen g => (Int, Int) -> Int -> g -> ((Int, Int) -> g -> (Int, g)) -> (Int, g)
sumRand r i g f = ((foldr (+) 0 (replicate i (fst (f r g)))), g)

-- 10.5.2 Rolling Dices
rollNMDices :: RandomGen g => (Int, Int) -> g -> Int -> Int -> Int
rollNMDices r g n m = foldr (+) 0 (replicate m (take n (randomRs r g)))