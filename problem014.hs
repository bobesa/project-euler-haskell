-- The following iterative sequence is defined for the set of positive integers:
-- 
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
-- 
-- Using the rule above and starting with 13, we generate the following sequence:
-- 
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
-- 
-- Which starting number, under one million, produces the longest chain?
-- 
-- NOTE: Once the chain starts the terms are allowed to go above one million.

collatz n len
	| n <= 1 = len+1
	| even n = collatz (n `div` 2) (len+1)
	| otherwise = collatz ((3*n)+1) (len+1)

getBigger x y = if snd x > snd y then x else y

getTopCollatz limit = f 1 (0,0)
	where
		f n best
			| n > limit = best
			| otherwise = f (n+1) (getBigger best (n,(collatz n 0)))
	

main = print $ getTopCollatz 1000000
