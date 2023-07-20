{--
The first two consecutive numbers to have two distinct prime factors are:
\begin{align}
14 &amp;= 2 \times 7\\
15 &amp;= 3 \times 5.
\end{align}
The first three consecutive numbers to have three distinct prime factors are:
\begin{align}
644 &amp;= 2^2 \times 7 \times 23\\
645 &amp;= 3 \times 5 \times 43\\
646 &amp;= 2 \times 17 \times 19.
\end{align}
Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
--}

import Primes (primes,factorize)

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x:xs) = x : (distinct $ dropWhile (==x) xs)

subtract' :: Ord a => [a] -> [a] -> [a]
subtract' (x:xs) (y:ys)
    | x == y = subtract' xs ys
    | x < y = x : subtract' xs (y:ys)
    | otherwise = subtract' (x:xs) ys

notPrimes :: [Int]
notPrimes = subtract' [1..] primes

partitionByFourConsecutive :: [Int] -> [[Int]]
partitionByFourConsecutive xs = let xs' = take 4 xs in if areConsecutive xs' then xs' : xss else xss
    where
        xss = partitionByFourConsecutive (tail xs)
        areConsecutive xs'' = foldr1 (&&) $ map (==1) $ zipWith (-) (tail xs'') xs''

main :: IO()
main = print $ head $ head $ filter allHaveFourFactors $ partitionByFourConsecutive notPrimes
    where allHaveFourFactors = foldr1 (&&) . map ((==4) . length . distinct . factorize)