{--
The Fibonacci sequence is defined by the recurrence relation:
$F_n = F_{n - 1} + F_{n - 2}$, where $F_1 = 1$ and $F_2 = 1$.
The $12$th term, $F_{12}$, is the first term to contain three digits.
What is the index of the first term in the Fibonacci sequence to contain $1000$ digits?
--}

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO()
main = print $ fst $ head $ dropWhile (let b = 10^999 in \(_ , f) -> 0 == div f b) $ zip [1..] fibs