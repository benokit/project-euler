-- If we list all the natural numbers below $10$ that are multiples of $3$ or $5$, we get $3, 5, 6$ and $9$. 
-- The sum of these multiples is $23$.
-- Find the sum of all the multiples of $3$ or $5$ below $1000$.

sumOfMultiples :: Int -> Int
sumOfMultiples n = sum $ takeWhile (< 1000) $ map (* n) [1,2..]

main :: IO ()
main = print $ sumOfMultiples 3 + sumOfMultiples 5 - sumOfMultiples 15