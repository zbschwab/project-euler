-- problem 6
{-
The sum of the squares of the first ten natural numbers is
    1^2 + 2^2 + .. + 10^2 = 385
The square of the sum of the first ten natural numbers is
    (1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the 
first ten natural numbers and the square of the sum is
    3025 - 385 = 2640

Find the difference between the sum of the squares of the 
first one hundred natural numbers and the square of the sum.
-}

sumSquares :: Integer
sumSquares =
    let sq x = x * x
        lst = map sq [2..100]
    in foldl (+) 1 lst

squareSum = foldl (+) 1 [2..100] ^2


main :: IO()
main = print (squareSum - sumSquares)