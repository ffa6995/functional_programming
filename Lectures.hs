-- Higher Order Functions

-- map
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
    then x : filter' f xs
    else filter' f xs

-- (.)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- uncurry 
-- uncurry f = \(x, y) -> f x y

-- curry
-- curry f = \x y -> f (x, y)


-- sieve of Eratostehnes
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)