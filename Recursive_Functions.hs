-- and
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == True = and' xs
            | x == False = False

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
-- merge :: Ord a => [a] -> [a] -> [a]