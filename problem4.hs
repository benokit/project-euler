-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two $2$-digit numbers is $9009 = 91 \times 99$.
-- Find the largest palindrome made from the product of two $3$-digit numbers.

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

isPalindrome :: Int -> Bool
isPalindrome n = foldr1 (&&) $ zipWith (==) ds (reverse ds)
    where ds = digits n


{--
product can be represented as (1000 - i) * (1000 - j) 1000000 - 1000 * (i + j) + i * j 
where i, j > 0 and j >= i (commutativity). 
The sequence of (i, j) that corresponds to a descending sequence of products can be constructed from 
an increasing sequence of sums q = i + j where for fixed q a subsequence of pairs (i, j) are ordered such that i * j is decreasing.
--}

f :: (Int,Int) -> Int
f (i, j) = 1000000 - 1000 * (i + j) + i * j

g :: Int -> [(Int,Int)]
g n = let q = div n 2 in takeWhile ((<) 0 . fst) $ iterate (\(i, j) -> (i - 1, j + 1)) (q, n - q)

main :: IO ()
main = print $ head $ filter isPalindrome $ map f $ foldr1 (++) $ map g [2, 3 ..]