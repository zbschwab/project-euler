-- problem 10
{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below 2 million.
-}

{- 
credit:  https://doisinkidney.com/posts/2018-11-10-a-very-simple-prime-sieve.html
            O'Neil (2009) [https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf]
            https://kowainik.github.io/posts/fixity
-}

main :: IO()
main = print (sum (takeWhile (< 2000000) primes))

-- set removal func that works if lists are ordered and contain no duplicates (sieve obeys this)
(\\) :: Ord a => [a] -> [a] -> [a]
[] \\ ys = []
xs \\ [] = xs
(x:xs) \\ (y:ys) = case compare x y of
    LT -> x:xs \\ (y:ys)  -- keep x, keep trying to remove y:ys from xs
    EQ -> xs \\ ys  -- remove x and y from both lists
    GT -> (x:xs) \\ ys  -- b/c ord, y can't match anything in x:xs, so delete and recurse over ys


-- priority queue to improve performance

infixr 5 :-  -- cons operator for our ordered list type


-- Queue is a root node with a min elm (key/val pair) and list of child queues
data Queue a b = Queue  -- gives params for func Queue
    { minKey :: !a  -- ! means evaluate immediately
    , minVal :: b
    , rest   :: List a b  -- list of other queues waiting to be merged
    }

-- linked list using Queue type
data List a b
    = Nil
    | (:-) {-# UNPACK #-} !(Queue a b)  
            (List a b)
    -- {} is compiler pragma saying Queue a b should be stored in memory 
    -- instead of as pointer (performance optimization)

-- merge queues (smallest minKey becomes new root, larger stored in rest list)
(<+>) :: Ord a => Queue a b -> Queue a b -> Queue a b
(<+>) q1@(Queue x1 y1 ts1) q2@(Queue x2 y2 ts2)
    | x1 <= x2 = Queue x1 y1 (q2 :- ts1)
    | otherwise = Queue x2 y2 (q1 :- ts2)

-- merges list of queues
mergeQs :: Ord a => List a b -> Queue a b
mergeQs (t :- ts) = mergeQs1 t ts
mergeQs Nil       = errorWithoutStackTrace "tried to merge empty list"

mergeQs1 :: Ord a => Queue a b -> List a b -> Queue a b
mergeQs1 t1 Nil              = t1
mergeQs1 t1 (t2 :- Nil)      = t1 <+> t2
mergeQs1 t1 (t2 :- t3 :- ts) = (t1 <+> t2) <+> mergeQs1 t3 ts

insert :: Ord a => a -> b -> Queue a b -> Queue a b
insert !k !v = (<+>) (singleton k v)

singleton :: a -> b -> Queue a b
singleton !k !v = Queue k v Nil


-- prime sieve

insertPrime :: (Ord b, Num b) => b -> [b] -> Queue b [b] -> Queue b [b]
insertPrime x xs = insert (x*x) (map (*x) xs)

adjust :: Ord a => a -> Queue a [a] -> Queue a [a]
adjust x q@(Queue y (z:zs) qs)
    | y <= x = adjust x (insert z zs (mergeQs qs))
    | otherwise = q

primes :: [Integer]
primes = 2 : sieve 3 (singleton 4 2)
    where
        adjust !x q@(Queue y z qs)
            | x < y = q
            | otherwise = adjust x (mergeQs1 (singleton (y + z) z) qs)
        sieve !x q
            | x < minKey q = x : sieve (x + 1) (insert (x * x) x q)
            | otherwise = sieve (x + 1) (adjust x q)