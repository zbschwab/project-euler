-- problem 7
{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, 13,
we can see that the 6th prime is 13.

What is the 10,001st prime?
-}

main :: IO ()
main = print (primes !! 10000)

primes = sieve [2..]

sieve (p:ps) = p : sieve [ x | x <- ps, mod x p /= 0 ]