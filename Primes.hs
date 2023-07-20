module Primes where

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

primes :: [Int]
primes = 2 : 3 : (subtract' [5..] (merge [[p * p, p * p + p..] | p <- primes]))

factorize :: Int -> [Int]
factorize n = fac n primes
    where
        fac 1 _ = [] 
        fac n' (p:ps) = if mod n' p == 0 then p : fac (div n' p) (p:ps) else fac n' ps