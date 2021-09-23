-- 4.6.5 Implementing safetail
-- Conditional Expression
safetail :: [a] -> [a]
safetail x = if null x then [] else tail x

-- Guarded Equations
safetail' :: [a] -> [a]
safetail' x | null x = []
           | otherwise = tail x

-- Pattern Matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

-- 4.6.6 Defining Functions
-- absolute between two numbers
abs' :: (Num a, Ord a) => a -> a 
abs' a = if a <= 0 then a * (-1) else a
absDif :: Num a => a -> a -> a
absDif a b = abs(a - b)

-- halve list
halve :: [a] -> ([a], [a])
halve a = (reverse (drop (length a `div` 2) (reverse a)), drop (length a `div` 2) a)