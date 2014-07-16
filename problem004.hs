-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

bottomLimit = 100
topLimit = 1000

-- Way #1: Functions etc...
isPalindromic num = (show num) == reverse (show num)

topPalindrome n1 n2 best = if (n1 == topLimit && n2 == topLimit) then best else (inc n1 n2 best)
	where
		inc n1 n2 best = if n1 < topLimit then (process (n1 + 1) n2 best) else (process bottomLimit (n2 + 1) best)
		process n1 n2 best = topPalindrome n1 n2 (if (isPalindromic (n1 * n2)) then (max best (n1 * n2)) else best)

-- main = print (topPalindrome bottomLimit bottomLimit 0)

-- Way #2: Array comprehension
main = print $ maximum [x * y | x <- [bottomLimit..topLimit], y <- [bottomLimit..topLimit], isPalindromic (x * y)]
