-- Problem #1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

main = print (multisum 1000 0 0)
	where
		multisum max current total = if (current == max) then total else (multisum max (current + 1) (if current `mod` 3 == 0 || current `mod` 5 == 0 then total + current else total)) 
