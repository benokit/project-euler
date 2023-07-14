{--
Let $d(n)$ be defined as the sum of proper divisors of $n$ (numbers less than $n$ which divide evenly into $n$).
If $d(a) = b$ and $d(b) = a$, where $a \ne b$, then $a$ and $b$ are an amicable pair and each of $a$ and $b$ are called amicable numbers.
For example, the proper divisors of $220$ are $1, 2, 4, 5, 10, 11, 20, 22, 44, 55$ and $110$; therefore $d(220) = 284$. The proper divisors of $284$ are $1, 2, 4, 71$ and $142$; so $d(284) = 220$.
Evaluate the sum of all the amicable numbers under $10000$.
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

divisors' :: Int -> [Int]
divisors' 1 = [1]
divisors' n = let 
    q = smallestDivisor n
    p = div n q
    ds = p : divisors p in 
        if n == q then [q, 1] else merge (map (* q) ds) ds

divisors :: Int -> [Int]
divisors n = (map divisors' [1..]) !! (n - 1)

ds :: [Int]
ds = 1 : map (sum . tail . divisors) [2..]

d :: Int -> Int
d n = ds !! (n - 1)

main :: IO ()
main = print $ sum $ filter (\n -> let q = d n in n /= q && n == d q) [1..10000]