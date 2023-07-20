{--
We shall say that an $n$-digit number is pandigital if it makes use of all the digits $1$ to $n$ exactly once. For example, $2143$ is a $4$-digit pandigital and is also prime.
What is the largest $n$-digit pandigital prime that exists?
--}

import Data.Numbers.Primes (isPrime)

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = f [] xs
    where
        f ys [] = []
        f ys' (y:ys) = (g y (ys' ++ ys)) ++ (f (ys' ++ [y]) ys)
        g y ys = map ((:) y) $ permutations ys

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

main :: IO()
main = print $ head $ filter isPrime $ map fromDigits $ foldr1 (++) $ map permutations [[7,6..1],[4,3..1]]