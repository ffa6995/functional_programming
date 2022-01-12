-- 14.7.3 Nim

-- Player data type
data Player = One | Two deriving (Show, Eq)

board :: [Int]
board = [5,4,3,2,1]

boardToString :: [Int] -> String
boardToString board = boardToStringRow board 1
    where
        boardToStringRow [] r = ""
        boardToStringRow (x:xs) r = show r ++ ": " ++ concat (replicate x "* ") ++ if null xs then "" else "\n" ++ boardToStringRow xs (r+1)

logic :: [Int] -> Int -> Int -> Maybe [Int]
logic b r s = if r > length b || r <= 0 || s <= 0 || b !! (r-1) < s then Nothing
                else Just (removeStarsFromRow b r s 0)
                    where
                        removeStarsFromRow [] _ _ _ = []
                        removeStarsFromRow (x:xs) r s cr = if cr == (r-1) then (x-s) : removeStarsFromRow xs r s (cr+1)
                                                             else x : removeStarsFromRow xs r s (cr+1)

changePlayer :: Player -> Player
changePlayer One = Two
changePlayer Two = One

gameFinished :: [Int] -> Bool
gameFinished [] = True
gameFinished (x:xs) = if x /= 0 then False else gameFinished xs 

nim :: IO ()
nim = do
    putStrLn "Start Nim"
    turn board One
    
    
turn :: [Int] -> Player -> IO ()
turn b p = do
    putStrLn ("Player " ++ show p ++ "'s turn")
    putStrLn (boardToString b)
    putStrLn "Pick one of the rows"
    row <- getLine
    putStrLn "How many stars you want to remove?"
    stars <- getLine
    let updatedBoard = logic b (read row) (read stars)
    case updatedBoard of
        Nothing -> do
            putStrLn "You can't do this, try something else ;)"
            turn b p
        (Just board) -> do
            if gameFinished board
                then do
                    putStrLn ("Player " ++ show p ++ " is the winner!")
                    putStrLn "Next game is starting . . ."
                    nim
                else
                    turn board (changePlayer p)

main :: IO ()
main = nim 
            
    