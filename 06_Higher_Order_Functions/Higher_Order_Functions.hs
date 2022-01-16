import Data.Char (toLower)

-- 6.7 Higher Order Functions

-- 6.7.2 Pipelining
multByIndex :: [Int] -> [Int]
multByIndex x = mult 1 x
    where
        mult :: Int -> [Int] -> [Int]
        mult _ [] = []
        mult i (x:xs) = x*i : mult (i+1) xs

removeOdd :: [Int] -> [Int]
removeOdd [] = []
removeOdd (x:xs) | even x = x : removeOdd xs
                 | odd x = removeOdd xs

pipeline :: [Int] -> Int
pipeline x = sum (removeOdd (multByIndex x))

pipeline' :: ([Int] -> [Int]) -> ([Int] -> [Int]) -> ([Int] -> Int) -> [Int] -> Int
pipeline' f g h x = h (g (f x))

pipeline'' :: (a -> b) -> (b -> c) -> (c -> d) -> a -> d
pipeline'' f g h x = h (g (f x))

-- 6.7.5 Decimals to Integer
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x+y) 0

-- 6.7.6 Palindrome Checker
palindrome :: String -> Bool
palindrome [] = True
palindrome x = str == reverse str
    where str = filter (/= ' ') (map toLower x)

-- 6.7.7 Unfolding
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
    | p x       = []
    | otherwise = h x : unfold p h t (t x)

-- redefine map f xs
map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f.head) tail

-- redefine iterate f
iterate' :: (a -> a) -> a -> [a]
iterate' f x = unfold (const False) id f x