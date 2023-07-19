{--
The number $3797$ has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: $3797$, $797$, $97$, and $7$. Similarly we can work from right to left: $3797$, $379$, $37$, and $3$.
Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
NOTE: $2$, $3$, $5$, and $7$ are not considered to be truncatable primes.
--}

import Data.Numbers.Primes (primes, isPrime)

digits :: Int -> [Int]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

fromDigits :: [Int] -> Int
fromDigits = foldr1 (\d a -> 10 * a + d)

test :: Int -> Bool
test n = let (d:ds) = digits n in g [d] ds
    where 
        g _ [] = True
        g as bs@(b:bs') = (isPrime $ fromDigits as) && (isPrime $ fromDigits bs) && (g (as ++ [b]) bs') 
    
main :: IO()
main = print $ sum $ take 11 $ filter test $ dropWhile (<10) primes