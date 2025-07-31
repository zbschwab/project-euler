-- problem 4
{-
A palindromic number reads the same both ways. The largest 
palindrome made from the product of two 2-digit numbers is
9009 = 91 x 99
Find the largest palindrome made from the product of two 
3-digit numbers.
-}

digitList :: Int -> [Int]
digitList n
    | n == 0    = []
    | otherwise = digitList (n `div` 10) ++ [n `mod` 10]

isPal :: Int -> Bool
isPal n = digitList n == reverse (digitList n)

findPal :: Int
findPal = maximum [x*y | x <- [100..999], y <- [x..999], isPal (x*y)]

main :: IO()
main = print findPal