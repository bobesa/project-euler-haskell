-- Problem #1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Way #1: Going from 0 to X (3 arguments)
multiSum1 max current total = if (current == max) then total else (multiSum1 max (current + 1) (if current `mod` 3 == 0 || current `mod` 5 == 0 then total + current else total)) 
-- main = print (multiSum1 1000 0 0)

-- Way #2: Going from X to 0 (only 2 arguments = faster?)
multiSum2 0 total = total
multiSum2 current total = multiSum2 (current - 1) (if current `mod` 3 == 0 || current `mod` 5 == 0 then total + current else total) 
main = print (multiSum2 999 0)
