n :: p -> Int
n a = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

mylast :: [a] -> a
mylast ls = head (reverse ls)

anotherlast :: [a] -> a
anotherlast ls = ls !! subtract 1 (length ls)

init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

myinit :: [a] -> [a]
myinit list = take (subtract 1 (length list)) list