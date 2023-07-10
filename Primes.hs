module Primes where

primes :: [Int]
primes = 2 : sieve [3,5..]
    where sieve (p:ns) = p : sieve [n | n <- ns, mod n p /= 0]

factorize :: Int -> [Int]
factorize n = fac n primes
    where
        fac 1 _ = [] 
        fac n' (p:ps) = if mod n' p == 0 then p : fac (div n' p) (p:ps) else fac n' ps