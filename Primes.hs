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

nextPrimeAndSieve :: (Int, [Int]) -> (Int, [Int])
nextPrimeAndSieve (p, sieve) = (p', sieve')
    where 
        ((p':_), sieve'') = dropWhileEqual ([p + 1, p + 2 ..], sieve)
        sieve' = mergeOrderedSequences sieve'' [p' * p', p' * p' + p' ..] 

primes :: [Int]
primes = map fst $ iterate nextPrimeAndSieve (2, [4, 6 ..])

factorize :: Int -> [Int]
factorize n = fac n primes
    where
        fac 1 _ = [] 
        fac n' (p:ps) = if mod n' p == 0 then p : fac (div n' p) (p:ps) else fac n' ps