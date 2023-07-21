{--
The prime $41$, can be written as the sum of six consecutive primes:
$$41 = 2 + 3 + 5 + 7 + 11 + 13.$$
This is the longest sum of consecutive primes that adds to a prime below one-hundred.
The longest sum of consecutive primes below one-thousand that adds to a prime, contains $21$ terms, and is equal to $953$.
Which prime, below one-million, can be written as the sum of the most consecutive primes?
--}

import Data.Numbers.Primes (primes, isPrime)

sums :: [Int] -> [[Int]]
sums ps = ps : zipWith (zipWith (+)) (sums ps) (tails ps)

tails :: [Int] -> [[Int]]
tails [a] = []
tails ps = let ps' = tail ps in ps' : tails ps'

main :: IO()
main = print $ head $ filter isPrime $ foldr1 (++) $ map reverse $ reverse $ sums ps
    where 
        ps = g 1000000 primes
        g s (p:ps) = let s' = s - p in if s' < 0 then [] else p : g s' ps 