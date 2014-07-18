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

collatz (n:xs)
	| n <= 1 = length $ n:xs
	| even n = collatz $ (n `div` 2):n:xs
	| otherwise = collatz $ ((3*n)+1):n:xs

getTopCollatz limit = f 1 (0,0)
	where
		f n best
			| n > limit = best
			| (collatz [n]) > (snd best) = f (n+1) (n,(collatz [n]))
			| otherwise = f (n+1) best
	

main = print $ getTopCollatz 1000000

-- THIS IS SUPER SLOW!!!
-- TODO: collatz will not be using array - just length of chain 
