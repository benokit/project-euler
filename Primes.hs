module Primes where

isInSieve :: Int -> [[Int]] -> Bool
isInSieve n sieve = or $ map ((==) n . head) sieve

rollSieve :: Int -> [[Int]] -> [[Int]]
rollSieve n sieve = map (\s -> if head s == n then tail s else s) sieve

primes = 2 : ps 3 [[4, 6 ..]]
    where ps n s = if isInSieve n s then ps (n + 1) (rollSieve n s) else n : ps (n + 1) (s ++ [[n * n, n * n + n ..]])

factorize :: Int -> [Int]
factorize n = fac n primes
    where
        fac 1 _ = [] 
        fac n' (p:ps) = if mod n' p == 0 then p : fac (div n' p) (p:ps) else fac n' ps