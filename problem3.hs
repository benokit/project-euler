-- The prime factors of $13195$ are $5, 7, 13$ and $29$.
-- What is the largest prime factor of the number $600851475143$?

import Primes (primes)

reduce :: (Int, [Int]) -> (Int, [Int])
reduce (1, ps) = (1, ps)
reduce (n, (p:ps)) = if mod n p == 0 then (div n p, (p:ps)) else (n, ps)

main :: IO ()
main = print $ head . snd . head . dropWhile ((/=) 1 . fst) $ iterate reduce (600851475143, primes)