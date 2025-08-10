-- problem 9
{-
A Pythagorean triplet is a set of 3 natural numbers,
a < b < c, for which a^2 + b^2 = c^2

There exists exactly one Pythagorean triplet for which
a + b + c = 1000. Find the product abc.
-}

main :: IO()
main = print findPy

findPy :: [Int]
findPy = [a*b*c | a <- [1..1000], b <- [1..1000], c <- [1..1000], a < b && b < c, a+b+c==1000, a*a+b*b==c*c]

