{--
The number, $197$, is called a circular prime because all rotations of the digits: $197$, $971$, and $719$, are themselves prime.
There are thirteen such primes below $100$: $2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79$, and $97$.
How many circular primes are there below one million?
--}

import Data.Numbers.Primes (primes, isPrime)

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

rotations :: Int -> [Int]
rotations x = x : (takeWhile (/=x) $ map fromDigits $ g (digits x))
    where g (x:xs) = let xs' = xs ++ [x] in xs' : g xs'

main :: IO()
main = print $ length $ filter (foldr1 (&&) . map isPrime . rotations) $ takeWhile (<1000000) primes