-- Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

limit = 4000000

-- Way #1: Functionalist
mainOld = print (evenfib 1 1 0)
	where 
		evenfib n1 n2 total = if n2 >= limit then total else (evenfib n2 (n1 + n2) (if (n1 + n2) `mod` 2 == 0 then total + (n1 + n2) else total))

-- Way #2: Array comprehension trick
-- We will test only first 100 fibonacci numbers
main = print (sum (filter above (take 100 fibs)))
	where
		above num = num < limit && even num 
		fibs = 1:2:[a + b | (a,b) <- zip fibs (tail fibs)]
