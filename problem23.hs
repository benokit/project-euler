{--
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of $28$ would be $1 + 2 + 4 + 7 + 14 = 28$, which means that $28$ is a perfect number.
A number $n$ is called deficient if the sum of its proper divisors is less than $n$ and it is called abundant if this sum exceeds $n$.

As $12$ is the smallest abundant number, $1 + 2 + 3 + 4 + 6 = 16$, the smallest number that can be written as the sum of two abundant numbers is $24$. By mathematical analysis, it can be shown that all integers greater than $28123$ can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
--}

import Primes (primes)

smallestDivisor :: Int -> Int
smallestDivisor n = head $ filter ((== 0) . (mod n)) primes

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y = x : merge (dropWhile (== x) xs) (y:ys)
    | x < y = y : merge (x:xs) (dropWhile (== y) ys)
    | otherwise = x : merge (dropWhile (== x) xs) (dropWhile (== y) ys)

divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = let 
    q = smallestDivisor n
    p = div n q
    ds = p : divisors p in 
        if n == q then [q, 1] else merge (map (* q) ds) ds

merge' :: [Int] -> [Int] -> [Int]
merge' (x:xs) (y:ys)
    | x < y = x : merge' (dropWhile (== x) xs) (y:ys)
    | x > y = y : merge' (x:xs) (dropWhile (== y) ys)
    | otherwise = x : merge' (dropWhile (== x) xs) (dropWhile (== y) ys)

abundantNumbers :: [Int]
abundantNumbers = filter (\n -> n < (sum $ tail $ divisors n)) [2..]

abundantSums :: [Int]
abundantSums = foldr1 (\(x:xs) ys -> x : merge' xs ys) [[(snd a) + b | b <- drop (fst a) $ abundantNumbers] | a <- zip [0..] abundantNumbers]

subtractSequence :: [Int] -> [Int] -> [Int]
subtractSequence xs [] = []
subtractSequence (x:xs) (y:ys)
    | x > y = subtractSequence (x:xs) ys
    | x < y = x : subtractSequence xs (y:ys)
    | otherwise = subtractSequence xs ys

main :: IO()
main = print $ sum $ subtractSequence [1..] (takeWhile (<=28123) abundantSums)