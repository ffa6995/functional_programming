{-# LANGUAGE DeriveFunctor #-}
import System.IO
import Control.Monad.Free
import Control.Monad.Trans.Writer 
import Control.Monad.Trans.Writer (Writer)
import Data.Bool (Bool)
-- 17.7 Free_Monads

-- 17.7.1 map fixed-point recursion
fix :: (a -> a) -> a
fix f = f (fix f)

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix (\rec (x:xs) -> if null xs then [f x] else f x : rec xs)

-- 17.7.2 Fibonacci Fixed-Point Recursion
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibFix :: Int -> Int
fibFix = fix (\rec a -> if a <= 1 then a else rec (a-1) + rec (a-2))

-- 17.7.3 cos Fixed-Point Recursion
fixCosIter :: Double -> [Double]
fixCosIter = fix (\rec a -> a : rec (cos a))

checkFixCos :: Double -> (Int, Double)
checkFixCos n = last (takeWhile (\(iterations,value) -> abs (value - snd (xs !! iterations)) > epsilon) xs)
    where {  xs = zip [1..] (fixCosIter n);
            epsilon = 0.0001;
            }

-- 17.7.4 Reproduce Hangman
-- 1. Define effect language
data HangmanEff a
  = PrintLine String a
  | ReadSecretLine a
  | Guess (Maybe String -> a)
  deriving Functor 

-- 2. Define a program type and smart constructors
type HangmanProgram = Free HangmanEff 

printLine :: String -> HangmanProgram ()
printLine str = liftF (PrintLine str ())

readSecretLine :: HangmanProgram ()
readSecretLine = liftF (ReadSecretLine ())

guess :: HangmanProgram (Maybe String)
guess = liftF (Guess id)

-- 3. Implement the logic the program type
hangman :: HangmanProgram ()
hangman = do
  printLine "Think of a word: "
  readSecretLine
  printLine "Try to guess it:"
  play 

play :: HangmanProgram ()
play = do 
  printLine "? "
  ret <- guess
  case ret of 
    Nothing -> printLine "You got it!"
    (Just diff) -> do
      printLine diff
      play

-- 4. Write an IO interpreter
interpretIO :: Maybe String -> HangmanProgram a -> IO a
interpretIO _ (Pure a) = return a
interpretIO s (Free (PrintLine str a)) = do
  putStrLn str
  interpretIO s a
interpretIO _ (Free (ReadSecretLine a)) = do
  secret <- sgetLine -- see slides on IO Monad
  interpretIO (Just secret) a
interpretIO (Just secret) (Free (Guess fa)) = do
  word <- getLine 
  if word == secret then
    interpretIO Nothing (fa Nothing)
  else
    interpretIO (Just secret) (fa (Just $ match secret word)) -- see slides on IO Monad

-- Write an IO interpreter
-- The action sgetLine reads a line of text from the keyboard echoing each character as a dash
sgetLine :: IO String
sgetLine = do
    x <- getCh -- see below
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- sgetLine
        return (x:xs)

-- The action getCh reads a single character from the keyboard without echoing it to the screen:
getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

-- The function match indicates which characters in one string occur in a second string
match :: String -> String -> String
match xs ys = map (\x -> if elem x ys then x else '-') xs

-- play: stack ghci Free_Monads.hs --resolver lts-17.13 --package free
-- interpretIO Nothing hangman

-- 17.7.5 Pure Hangman Interpreter
-- stack ghci Free_Monads.hs --resolver lts-17.13 --package free --package transformers
pureInterpret :: String -> [String] -> HangmanProgram a -> Writer String a
pureInterpret word _ (Pure h) = return h
pureInterpret word guesses (Free (PrintLine s a)) = tell (s ++ "\n") >> pureInterpret word guesses a
pureInterpret word guesses (Free (ReadSecretLine a)) = tell (map (const '-') word ++ "\n") >> pureInterpret word guesses a
pureInterpret word (x:xs) (Free (Guess g)) = tell (x ++ "\n") >> if x == word then
                                                        pureInterpret word [] (g Nothing)
                                                        else pureInterpret word xs (g (Just $ match word x))

runPureInterpret :: ((), String)
runPureInterpret = runWriter $ pureInterpret "Haskell" ["H", "l", "Has", "Haskell"] hangman

printPureInterpet :: IO ()
printPureInterpet = putStr (snd runPureInterpret)

testPureInterpet :: Bool
testPureInterpet = snd runPureInterpret == "Think of a word: \n-------\nTry to guess it:\n? \nH\nH------\n? \nl\n-----ll\n? \nHas\nHas----\n? \nHaskell\nYou got it!\n"

-- 17.7.6 Nim Free Monads
-- Define the effect language
data NimEff a
    = NimPrintLine String a
    | NimGetLine (String -> a)
    deriving Functor
  
--Define a program type and smart constructors
type NimProgram = Free NimEff
nimPrintLine :: String -> NimProgram ()
nimPrintLine str = liftF (NimPrintLine str ())

nimGetLine :: NimProgram String
nimGetLine = liftF (NimGetLine id)

board :: [Int]
board = [5,4,3,2,1]

-- Player data type
data Player = One | Two deriving (Show, Eq)

changePlayer :: Player -> Player
changePlayer One = Two
changePlayer Two = One

logic :: [Int] -> Int -> Int -> Maybe [Int]
logic b r s = if r > length b || r <= 0 || s <= 0 || b !! (r-1) < s then Nothing
                else Just (removeStarsFromRow b r s 0)
                    where
                        removeStarsFromRow [] _ _ _ = []
                        removeStarsFromRow (x:xs) r s cr = if cr == (r-1) then (x-s) : removeStarsFromRow xs r s (cr+1)
                                                             else x : removeStarsFromRow xs r s (cr+1)


gameFinished :: [Int] -> Bool
gameFinished [] = True
gameFinished (x:xs) = if x /= 0 then False else gameFinished xs 

boardToString :: [Int] -> String
boardToString board = boardToStringRow board 1
    where
        boardToStringRow [] r = ""
        boardToStringRow (x:xs) r = show r ++ ": " ++ concat (replicate x "* ") ++ if null xs then "" else "\n" ++ boardToStringRow xs (r+1)


nim :: NimProgram ()
nim = do 
  nimPrintLine "Start Nim"
  turn board One

turn :: [Int] -> Player -> NimProgram ()
turn b p = do
    nimPrintLine ("Player " ++ show p ++ "'s turn")
    nimPrintLine (boardToString b)
    nimPrintLine "Pick one of the rows"
    row <- nimGetLine
    nimPrintLine "How many stars you want to remove?"
    stars <- nimGetLine
    let updatedBoard = logic b (read row) (read stars)
    case updatedBoard of
        Nothing -> do
            nimPrintLine "You can't do this, try something else ;)"
            turn b p
        (Just board) -> do
            if gameFinished board
                then do
                    nimPrintLine ("Player " ++ show p ++ " is the winner!")
                    nimPrintLine "Next game is starting . . ."
                    nim
                else
                    turn board (changePlayer p)


nimInterpretIO :: NimProgram a -> IO a
nimInterpretIO (Pure a) = return a
nimInterpretIO (Free (NimPrintLine str a)) = do
  putStrLn str
  nimInterpretIO a
nimInterpretIO (Free (NimGetLine fa)) = do
  line <- getLine
  nimInterpretIO (fa line)