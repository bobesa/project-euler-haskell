-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters 
-- and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

strSingles = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
strBelow20 = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
strTens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

getNumber n
	| n < 10 = strSingles !! n
	| n < 20 = strBelow20 !! (n-10)
	| n < 100 = strTens !! (n `div` 10) ++ (if n `mod` 10 > 0 then "" ++ strSingles !! (n `mod` 10) else "" )
	| n < 1000 = strSingles !! (n `div` 100) ++ "hundred" ++ (if n `mod` 100 > 0 then "and" ++ (getNumber (n `mod` 100)) else "" )
	| otherwise = "Onethousand"

main = print $ sum [length $ getNumber x | x <- [1..1000]]
