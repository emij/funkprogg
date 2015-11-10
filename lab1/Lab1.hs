module Lab1 where
import Test.QuickCheck

{-
    Assignment 1
    Group 46
    David Göransson
    Emil Johansson
-}

{-
    Part 1
    Number of steps is k+1
    The base case is k == 0, which is one step,
    and we will get one more step for every increment
    of k.
-}


{-
    Part 2
-}

power1 :: Integer -> Integer -> Integer
power1 n k
    | k == 0 = 1
    | k > 0 = product (listProduct n k)

listProduct :: Integer -> Integer -> [Integer]
listProduct n k =  replicate (fromInteger k) n

prop_power1 n k = let k' = abs k in
                    n^k' ==  power1 n k'

{-
    Part 3
-}

power2 :: Integer -> Integer -> Integer
power2 n k
    | k == 0 = 1
    | even k = power2 (n*n) (k `div` 2)
    | otherwise = n * power2 n (k-1)

prop_power2 n k = let k' = abs k in
                    n^k' == power2 n k'

{-
    Part 4
    We have chosen numbers between zero and fifty. We do not use any negative
    numbers for k since our function is not defined for that input. Test
    cases thus uses all combinations of negative, zero and positive number
    that the function should support. Already from the start we implemented
    property functions for each top level function and verified their
    functionallity with QuickCheck.
-}

prop_powers n k = let k' = abs k in
                    n^k' == power1 n k' && n^k' == power2 n k'

test_powers =
    and [ prop_powers x y | x <- [-1,0,1], y <- [0,1] ]




