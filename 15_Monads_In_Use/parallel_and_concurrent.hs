import Data.IORef (readIORef, newIORef, IORef, atomicModifyIORef)
import Control.Concurrent.Async (wait, async)
import GHC.IO.Handle (hSetBuffering, BufferMode (LineBuffering))
import Control.Concurrent (newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM, replicateM_, when, join, forM)
import System.IO (stdout)
import Control.Concurrent.STM (newTChanIO, writeTChan, isEmptyTChan, readTChan, atomically, TChan, STM)

-- 15.3.1 Primes IORef
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..k - 1], k `mod` x == 0] else False

primesRef :: Int -> IO () 
primesRef a = do
    ref <- newIORef []
    -- spawn the threads with async
    as <- mapM (async . primesThread ref) [2..a]
    -- wait for all the threads
    mapM_ wait as 
    -- read the results
    primes <- readIORef ref
    print primes

primesThread :: IORef [Int] -> Int -> IO()
primesThread ref n = atomicModifyIORef ref (\acc -> (if isPrime n then acc ++ [n] else acc, ()))

-- 15.3.2 Primes with MVar
primesMVar :: Int -> Int -> IO ()
primesMVar n c = do 
    hSetBuffering stdout LineBuffering
    -- create a new empty MVar
    var <- newEmptyMVar
    -- create consumer
    cs <- replicateM c (async $ consume var)
    -- create producer
    produce var n 1 c
    mapM_ wait cs

produce :: MVar (Maybe Int) -> Int -> Int -> Int -> IO ()
produce var n cNum cons = if cNum < n then do
            putMVar var (Just (cNum + 1))
            produce var n (cNum + 1) cons
        else do
            putMVar var Nothing
            when (cons > 0) (produce var n cNum (cons - 1)) -- communicate end of computation to the consumers

consume :: MVar (Maybe Int) -> IO ()
consume var = do
    n <- takeMVar var
    case n of
        Nothing -> return ()
        (Just x) -> do
            when (isPrime x) (print x)
            consume var

-- 15.3.3 Primes with TChan 
-- https://hackage.haskell.org/package/stm-2.1/docs/Control-Concurrent-STM-TChan.html
primesTChan :: Int -> Int -> IO()
primesTChan n c = do
    natNums <- newTChanIO
    primes <- newTChanIO
    -- write nat nums into natNums TChan
    let writeNatNums m = do
        writeTChan natNums m
        when (m < n) (writeNatNums (m + 1))
    atomically $ writeNatNums 1
    -- create consumers
    cs <- replicateM c (async $ atomically $ consumeTChan natNums primes)
    mapM_ wait cs
    let consume = do
        empty <- atomically $ isEmptyTChan primes
        if empty then do
            return ()
        else do
            a <- atomically $ readTChan primes
            print a
            consume
    consume 

consumeTChan :: TChan Int -> TChan Int -> STM ()
consumeTChan nums primes = do
    empty <- isEmptyTChan nums
    if empty then do
        return ()
    else do
        a <- readTChan nums
        when (isPrime a) (writeTChan primes a)
        consumeTChan nums primes