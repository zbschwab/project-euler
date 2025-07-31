-- problem 5
{-
2520 is the smallest number that can be divided by each of the numbers from 
1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 
1 to 20?
-}

-- gcd w/ euclidean algorithm
myGCD :: (Ord a, Num a) => a -> a -> a
myGCD a b
    | a == b    = a
    | a > b     = myGCD (a-b) b
    | a < b     = myGCD a (b-a)

-- lcm(a,b) = (a * b) / gcd(a,b)
myLCM :: Integral a => a -> a -> a
myLCM a b = (a * b) `div` myGCD a b

main :: IO ()
main = print (foldl myLCM 1 [2..20])