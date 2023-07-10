-- The sum of the squares of the first ten natural numbers is,
-- $$1^2 + 2^2 + ... + 10^2 = 385.$$
-- The square of the sum of the first ten natural numbers is,
-- $$(1 + 2 + ... + 10)^2 = 55^2 = 3025.$$
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is $3025 - 385 = 2640$.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

{--
    q_n = 1 + 2 + ... + n = n * (n + 1) / 2
    p_n = 1^2 + 2^2 + ... + n^2 = n * (2 * n^2 + 3 * n + 1) / 6
--}

q :: Int -> Int
q n = div (n * (n + 1)) 2

p :: Int -> Int
p n = div (n * (2 * n * n + 3 * n + 1)) 6

main :: IO ()
main = print $ q100 * q100 - (p 100)
    where q100 = q 100 
