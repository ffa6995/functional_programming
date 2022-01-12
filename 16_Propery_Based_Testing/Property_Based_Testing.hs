import Test.QuickCheck
import System.Random

-- Property_Based_Testing
-- 16.6.1 Checking msort

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x > y = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

-- msort 
msort :: Ord a => [a] -> [a]
msort [] = []
msort x | length x <= 1 = x
        | otherwise = merge (msort (firsthalf x)) (msort (secondhalf x))

firsthalf :: [a] -> [a]
firsthalf [] = []
firsthalf x = take (length x `div` 2) x

secondhalf :: [a] -> [a]
secondhalf [] = []
secondhalf x = drop (length x `div` 2) x

-- test: isSorted (msort [5,2,1,6,3,8]) = True, isSorted ([5,2,1,6,3,8]) = False
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xx:xs) = (x <= xx) && isSorted (xx:xs)

-- test: quickCheck prop_issorted
prop_issorted :: Ord a => [a] -> Property
prop_issorted x = property (isSorted x)

-- 16.6.2 verify algebraic laws of addition
-- associative addition for Int -- quickCheck prop_associative_add 
prop_associative_add :: Int -> Int -> Int -> Property
prop_associative_add u v w = property ((u + v) + w == u + (v + w))

-- commutative addition for Int -- quickCheck prop_commutative_add 
prop_commutative_add :: Int -> Int -> Property
prop_commutative_add u v = property (u + v == v + u)

-- associative addition for Float -- quickCheck prop_associative_add_float
-- fails in some cases 
prop_associative_add_float :: Float -> Float -> Float -> Property
prop_associative_add_float u v w = property ((u + v) + w == u + (v + w))

-- commutative addition for Float -- quickCheck prop_commutative_add_float
prop_commutative_add_float :: Float -> Float -> Property
prop_commutative_add_float u v = property (u + v == v + u)

-- 16.6.3 Dice

newtype Dice = Roll Int deriving Show

genDice :: Gen Dice
genDice = elements [Roll 1, Roll 2, Roll 3, Roll 4, Roll 5, Roll 6]

instance Arbitrary Dice where
    arbitrary = genDice

-- coverage
-- test quickCheck (checkCoverage checkCoverageTwoDices)
checkCoverageTwoDices :: Dice -> Dice -> Property
checkCoverageTwoDices (Roll a) (Roll b) = cover 16.67 (a + b == 7) "of the sum of two Dices are 7" (a + b == a + b)

