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