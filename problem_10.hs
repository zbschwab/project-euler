-- problem 10
{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below 2 million.
-}

main :: IO ()
main = print (takeWhile (< 2000000) primes)

primes = sieve [2 ..]

sieve (p : ps) = p : sieve [x | x <- ps, mod x p /= 0]
