-- By listing the first six prime numbers: $2, 3, 5, 7, 11$, and $13$, we can see that the $6$th prime is $13$.
-- What is the $10\,001$st prime number?

import Primes (primes)

main :: IO ()
main = print $ head $ drop 10000 primes