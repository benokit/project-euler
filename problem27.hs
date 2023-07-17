{--
Euler discovered the remarkable quadratic formula:
$n^2 + n + 41$
It turns out that the formula will produce $40$ primes for the consecutive integer values $0 \le n \le 39$. However, when $n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41$ is divisible by $41$, and certainly when $n = 41, 41^2 + 41 + 41$ is clearly divisible by $41$.
The incredible formula $n^2 - 79n + 1601$ was discovered, which produces $80$ primes for the consecutive values $0 \le n \le 79$. The product of the coefficients, $-79$ and $1601$, is $-126479$.
Considering quadratics of the form:

$n^2 + an + b$, where $|a| &lt; 1000$ and $|b| \le 1000$where $|n|$ is the modulus/absolute value of $n$e.g. $|11| = 11$ and $|-4| = 4$

Find the product of the coefficients, $a$ and $b$, for the quadratic expression that produces the maximum number of primes for consecutive values of $n$, starting with $n = 0$.
--}

import Primes (primes)
import Data.Map (Map, fromList, member)

isPrime :: Int -> Bool
isPrime n = let q = fromList [(p, True) | p <- takeWhile (<20000) primes] in member n q

bs :: [Int]
bs = takeWhile (<1000) primes

as :: Int -> [Int]
as b = filter (\n -> isPrime (b + n + 1)) [-b..999]

cp :: Int -> Int -> Int
cp a b = length $ takeWhile isPrime [n * (n + a) + b | n <- [2..]]

main :: IO()
main = print $ fst $ foldr1 (\(i,c) (i',c') -> if c > c' then (i, c) else (i', c')) [(a * b, cp a b) | b <- bs, a <- as b]