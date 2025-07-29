-- problem 5
{-
2520 is the smallest number that can be divided by each of the numbers from 
1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 
1 to 20?
-}

-- 1-5 : 30
-- 1-6 : 60
-- m : max number in 1, 2,..., m sequence
-- n : smallest number evenly divisble by 1-m
-- d : current divisor

main :: IO ()
main = print (evendiv 6 6 6)


evendiv n m d
    | d <= 1            = n
    | n `mod` d == 0    = evendiv n m (d - 1)
    | otherwise         = evendiv (n + m) m d
