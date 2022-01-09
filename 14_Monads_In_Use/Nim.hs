-- 14.7.3 Nim

-- Player data type
data Player = One | Two deriving (Show, Eq)

board :: [Int]
board = [5,4,3,2,1]

boardToString :: [Int] -> String
boardToString board = boardToStringRow board
    where
        boardToStringRow [] = ""
        boardToStringRow (x:xs) = show x ++ ": " ++ concat (replicate x "* ") ++ if null xs then "" else "\n" ++ boardToStringRow xs 

logic :: [Int] -> Int -> Int -> Maybe []
logic b r s = if r > length b || r <= 0 || s <= 0 || b !! (r-1) < s then Nothing
                else Just (removeStarsFromRow b r s 0)
                    where
                        removeStarsFromRow [] _ _ _ = []
                        removeStarsFromRow (x:xs) r s cr = if cr == (r-1) then (x-1) : removeStartsFromRow xs r s (cr+1)
                                                             else x : removeStarsFromRow xs r s (cr+1)