module Primes where

mergeOrderedSequences :: Ord a => [a] -> [a] -> [a]
mergeOrderedSequences (x:xs) (y:ys)
    | x < y = x : mergeOrderedSequences xs (y:ys)
    | x > y = y : mergeOrderedSequences (x:xs) ys
    | otherwise = x : mergeOrderedSequences xs ys

dropWhileEqual :: Eq a => ([a], [a]) -> ([a], [a])
dropWhileEqual q@((x:xs), (y:ys))
    | x == y = dropWhileEqual (xs, ys)
    | otherwise = q

nextPrimeAndSieve :: (Integer, [Integer]) -> (Integer, [Integer])
nextPrimeAndSieve (p, sieve) = (p', sieve')
    where 
        (n, s) = dropWhileEqual ([p + 1, p + 2 ..], sieve)
        p' = head n
        sieve' = mergeOrderedSequences s [p' * p', p' * p' + p' ..] 

primes :: [Integer]
primes = map fst $ iterate nextPrimeAndSieve (2, [4, 6 ..])