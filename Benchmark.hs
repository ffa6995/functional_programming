#!/usr/bin/env stack
-- stack --resolver lts-16.14 script

-- NOTE: to run type into your console
-- stack Benchmark.hs

-- NOTE: need to import criterion dependencies
import qualified Criterion.Main as Crit

-- NOTE: fully evaluates both arguments
strictConj :: Bool -> Bool -> Bool
strictConj True False  = False
strictConj False True  = False
strictConj False False = False 
strictConj True True   = True

-- NOTE: avoids evaluating the 2nd argument,
-- is therefore NOT strict on the 2nd argument
-- has the consequene that if the first argument
-- evaluates to False the 2nd argument is not evaluated
(&&) :: Bool -> Bool -> Bool
(&&) False _  = False
(&&) True  b  = b

-- NOTE: due to strict && it cannot short circuit and therefore
-- will evaluate the WHOLE list, therefore will take same time
-- independent if there is a False 
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = strictConj x (Main.and xs)

-- NOTE: using non-strict &&, therefore upon False will 
-- short-circuit and not traverse full list because
-- will not evaluate recursive case
and :: [Bool] -> Bool
and []     = True
and (x:xs) = x Main.&& Main.and xs

-- NOTE: upon False will short-circuit and not traverse full 
-- list because will not evaluate recursive case
andShort :: [Bool] -> Bool
andShort []     = True
andShort (x:xs) = if x then andShort xs else False

main :: IO ()
main = do
    Crit.defaultMain [
        -- every all True should result in roughly the same performance (~1.5sec)
        Crit.bgroup "and performance"
        -- ~1.465 sec
        [ Crit.bench "all True standard and"  $ Crit.nf Main.and (True : replicate 10000000 True)
        -- ~1.084 sec (no function call, therefore faster)
        , Crit.bench "all True short and"  $ Crit.nf Main.andShort (True : replicate 10000000 True)
        -- ~1.454 sec (about the same as in standard and)
        , Crit.bench "all True strict and"  $ Crit.nf Main.and' (True : replicate 10000000 True)

        -- ~ 137 nano sec (short circuit due to lazy evaluation)
        , Crit.bench "first False standard and"  $ Crit.nf Main.and (False : replicate 10000000 True)
        -- ~ 101 nano sec (short circuit due to lazy evaluation)
        , Crit.bench "first False short and"  $ Crit.nf Main.andShort (False : replicate 10000000 True)
        -- ~ 1.556 sec (no short circuit, due to strict evaluation)
        , Crit.bench "first False strict and"  $ Crit.nf Main.and' (False : replicate 10000000 True) ]
      ]