-- and
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x = and' xs
            | not x = False

-- concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' ([]:vs) = concat' vs
concat' ((x:xs):vs) = x:concat' (xs:vs)

-- replicate
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' x y = y : replicate' (x-1) y

-- (!!) nth element of a list
elementN :: [a] -> Int -> a
elementN [] y = error "index too large"
elementN x 0 = head x
elementN (x:xs) y = elementN xs (y-1)

-- elem
-- elem' :: Eq a => a -> [a] -> Bool


-- merge sort
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