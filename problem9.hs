-- A Pythagorean triplet is a set of three natural numbers, $a \lt b \lt c$, for which,
-- $$a^2 + b^2 = c^2.$$
-- For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.
-- There exists exactly one Pythagorean triplet for which $a + b + c = 1000$. Find the product $abc$.

{--
    a^2 + b^2 = (10^3 - (a + b))^2 
    10^6 - 2 * 10^3 * (a + b) + 2 * a * b = 0
    b = (5 * 10^5 - 10^3 * a) / (10^3 - a) 
--}

a = head $ dropWhile (\n -> mod (500000 - 1000 * n) (1000 - n) /= 0) [1,2..]

b = div (500000 - 1000 * a) (1000 - a)

c = 1000 - a - b

main :: IO ()
main = print $ a * b * c
