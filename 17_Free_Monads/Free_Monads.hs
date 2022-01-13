{-# LANGUAGE DeriveFunctor #-}
import System.IO
import Control.Monad.Free

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
pureInterpret :: String -> [String] -> HangmanProgram a -> Writer String a
pureInterpret word _ (Pure h) = return h
pureInterpret word guess (Free (PrintLine s a)) = tell (s ++ "\n") >> pureInterpret

-- 17.7.6 Nim Free Monads