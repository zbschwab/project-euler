-- problem 3
{-
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143?
-}

-- tree
--   case 1: n mod p = 0 and n/p /= 1 -->  branch p, branch n/p
--   case 1: n mod p = 0 and n/p = 1  -->  n is prime


main :: IO ()
main = print (pfac 600851475143 2)
pfac n p
    | p * p > n         = n
    | n `mod` p == 0    = pfac (n `div` p) p
    | otherwise         = pfac n (p + 1)
