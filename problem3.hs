-- The prime factors of $13195$ are $5, 7, 13$ and $29$.
-- What is the largest prime factor of the number $600851475143$?

import Primes (factorize)

main :: IO ()
main = print $ last $ factorize 600851475143