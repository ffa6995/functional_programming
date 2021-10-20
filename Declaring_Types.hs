-- Declaring Types

-- Declaring Types

-- 7.8.1 Weekdays
-- Weekday is a type with five data constructors.
data Weekday =
     Monday
   | Tuesday
   | Wednesday
   | Thursday
   | Friday
   | Saturday
   | Sunday

-- 7.8.2 Nat multiplication using recursion and add
data Nat = Zero
      | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add Zero n = n
add (Succ m) n = Succ (add m n)

natMult :: Nat -> Nat -> Nat
natMult m Zero = Zero
natMult m (Succ n) = add (natMult m n) m