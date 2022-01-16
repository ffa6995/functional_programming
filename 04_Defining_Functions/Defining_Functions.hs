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
abs' :: Int -> Int
abs' a = if a <= 0 then a * (-1) else a
absDif :: Int -> Int -> Int
absDif a b = abs(a - b)

-- halve list
halve :: [a] -> ([a], [a])
halve a = (reverse (drop (length a `div` 2) (reverse a)), drop (length a `div` 2) a)

-- function that solves the quadratic equation ax2+bx+c=0
quadEq :: (Float, Float, Float) -> (Float, Float)
quadEq (a, b, c) = (x1, x2)
    where
        x1 = e + sqrt d / (2 * a)
        x2 = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)

-- median
median :: [Int] -> Int
median x | length x `div` 2 /= 0 = (x !! (length x `div` 2) + x !! (length x `div` 2 - 1)) `div` 2
         | otherwise = x !! (length x `div` 2)

-- sumdown - returns sum of non negative integers
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- euclid - calculates the greatest common divisor of two non-negative integers
euclid :: Int -> Int -> Int
euclid x y | x == y = y
           | x < y = euclid x (y-x)
           | x > y = euclid (x-y) y

-- consecutive duplicates - eliminate repeated elements
compress :: [Char] -> [Char]
compress [] = []
compress (x:xs) | null xs = x : xs
                | x == head xs = compress xs
                | otherwise = x : compress xs

-- packing - pack consecutive duplicates in sublists
pack :: [Char] -> [String]
pack [] = []
pack (x:xs) = (x:first) : pack rest
         where
           pack' [] = ([], [])
           pack' (y:ys)
                   | y == x = let (f,r) = pack' ys in (y:f, r)
                   | otherwise = ([], (y:ys))
           (first,rest) = pack' xs

-- run-lenght encoding - encodes string to tuples with number of occurence and char
encode :: String -> [(Int, Char)]
encode [] = []





