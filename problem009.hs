-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, 
-- a2 + b2 = c2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

sizes = [1..500]
total = 1000
main = print $ maximum [product [a,b,c] | a <- sizes, b <- sizes, let c = (total - b - a), (a^2)+(b^2) == (c^2)]
