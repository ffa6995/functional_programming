-- 12. Monads Defined

--12.12.1 CPS quadratic equation
quadEq :: (Float, Float, Float) -> (Float, Float)
quadEq (a, b, c) = (x1, x2)
    where
        x1 = e + sqrt d / (2 * a)
        x2 = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)

calcD :: (Float, Float, Float) -> Float
calcD (a, b, c) = b * b - 4 * a * c

calcE :: (Float, Float) -> Float
calcE (a, b) = - b / (2 * a)

quadtraticEq :: (Float, Float, Float) -> Float -> Float -> (Float, Float)
quadtraticEq (x, y, z) d e = (e + sqrt d / (2 * x), e - sqrt d / (2 * x))

quadratic_cps :: (Float, Float, Float) -> (Float, Float)
quadratic_cps (x, y, z) =
  calcD (x, y, z) $ \d ->
  calcE (x, y) $ \e ->
  quadtraticEq (x, y, z) d e
  



--12.12.2 Monad Basics

--12.12.3