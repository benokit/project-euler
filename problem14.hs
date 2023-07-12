{--
The following iterative sequence is defined for the set of positive integers:

$n \to n/2$ ($n$ is even)
$n \to 3n + 1$ ($n$ is odd)
Using the rule above and starting with $13$, we generate the following sequence:
$$13 \to 40 \to 20 \to 10 \to 5 \to 16 \to 8 \to 4 \to 2 \to 1.$$
It can be seen that this sequence (starting at $13$ and finishing at $1$) contains $10$ terms. 
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at $1$.
Which starting number, under one million, produces the longest chain?
NOTE: Once the chain starts the terms are allowed to go above one million.
--}

cSeq :: Int -> [Int]
cSeq 1 = [1]
cSeq n 
    | mod n 2 == 0 = n : cSeq (div n 2)
    | otherwise = n : cSeq (3 * n + 1)

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f (x:xs) = g (x, (f x)) xs 
    where 
        g (x',_) [] = x'
        g q@(x',fx') (x'':xs'') = let fx'' = f x'' in g (if fx'' > fx' then (x'', fx'') else q) xs''

main :: IO ()
main = print $ maxBy (length . cSeq) [1..1000000]