-- 14.7.2 Key Value Store IO
data KeyValueStore k v = Empty
                   | Store k v (KeyValueStore k v)
                   deriving Show

keyValueStore :: KeyValueStore Int String
keyValueStore = Store 1 "ABC" (Store 2 "DEF" Empty)

printKeyValueStore :: (Show k, Show v) => KeyValueStore k v -> IO()
printKeyValueStore Empty = return ()
printKeyValueStore (Store k v next) = do
    putStr (show k)
    putStr " : "
    putStr (show v)
    putStr "\n"
    printKeyValueStore next

