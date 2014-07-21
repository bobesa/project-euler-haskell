-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

import Data.Numbers.Primes

limit = 20

isDividable n = length [d | d <- [1..limit], n `mod` d == 0] == limit

f x n = if isDividable c then c else f x (n+1)
	where
		c = x * n

main = print $ f (foldl1 (*) $ takeWhile (<limit) primes) 1
