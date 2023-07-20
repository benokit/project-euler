{--
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
\begin{align}
9 = 7 + 2 \times 1^2\\
15 = 7 + 2 \times 2^2\\
21 = 3 + 2 \times 3^2\\
25 = 7 + 2 \times 3^2\\
27 = 19 + 2 \times 2^2\\
33 = 31 + 2 \times 1^2
\end{align}
It turns out that the conjecture was false.
What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
--}

import Primes (primes)

oddComposites :: [Int]
oddComposites = subtract' [9,11..] primes

subtract' :: Ord a => [a] -> [a] -> [a]
subtract' (x:xs) (y:ys)
    | x == y = subtract' xs ys
    | x < y = x : subtract' xs (y:ys)
    | otherwise = subtract' (x:xs) ys

merge' :: Ord a => [a] -> [a] -> [a]
merge' (x:xs) (y:ys)
    | x == y = x : merge' xs ys
    | x < y = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys

merge ::Ord a => [[a]] -> [a]
merge (xs:(y:ys):xss) = takeWhile (<y) xs ++ merge' (dropWhile (<y) xs) (merge ((y:ys):xss))

main :: IO()
main = print $ head $ subtract' oddComposites $ merge [[p + q | p <- primes ] | q <- map (*2) $ zipWith (*) [1..] [1..]]