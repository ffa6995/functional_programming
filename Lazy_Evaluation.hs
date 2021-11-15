-- install stack: https://docs.haskellstack.org/en/stable/README/
-- windows installer: https://get.haskellstack.org/stable/windows-x86_64-installer.exe


-- 8.7.4 lazy fibonacci
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

ones :: [Int]
ones = 1 : ones


    -- The first two numbers are 0 and 1.
    -- The next is the sum of the previous two.
    -- Return to step 2.
