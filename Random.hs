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

-- 10.5.2.1 Rolling Dices
-- test: rollNMDices (1,6) (mkStdGen 33) 5 5
-- rollNMDices (1,6) (mkStdGen 42) 5 5
rollNMDices :: RandomGen g => (Int, Int) -> g -> Int -> Int -> [Int]
rollNMDices r g n m | m == 0 = []
                    | otherwise = sum (take n (randomRs r g)) : rollNMDices r (snd (split g)) n (m-1)

-- 10.5.2.2 avg2Dices
-- test: avg2Dices 33 5
avg2Dices :: Int -> Int -> Double
avg2Dices s m =  fromIntegral (sum (rollNMDices (1,6) (mkStdGen s) 2 m) `div` m)

-- 10.5.2.3 replicate2Dices
replicate2Dices :: Int -> Int -> Double
replicate2Dices s n = replicate2DicesAvgs s n / fromIntegral n

replicate2DicesAvgs :: Int -> Int -> Double
replicate2DicesAvgs s n | n == 0 = 0
                    | otherwise = avg2Dices s 1000 + replicate2DicesAvgs (s`div`n*15) (n-1)

-- What is the result and why? replicate2Dices 42 10000
-- Ein Würfel ist gleichverteilt und deshalb kommt bei einer Großen Anzahl an Würfen
-- von jeder Zahl etwa gleich viel, deshalb ist das Ergebnis mit zwei Würfeln 6 
