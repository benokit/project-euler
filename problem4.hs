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
product can be represented as (1000 - i) * (1000 - j) = 1000000 - 1000 * (i + j) + i * j 
where i > 0 and j >= i (commutativity). 
The sequence of (i, j) that corresponds to a descending sequence of products can be constructed from 
an increasing sequence of sums q = i + j where for fixed q a subsequence of pairs (i, j) are ordered such that i * j is decreasing.
--}

prod :: (Int,Int) -> Int
prod (i, j) = (1000 - i) * (1000 - j)

subsequenceForSum :: Int -> [(Int,Int)]
subsequenceForSum s = let q = div s 2 in [(q - i, s - q + i) | i <- [0 .. (q - 1)]]

main :: IO ()
main = print $ head $ filter isPalindrome $ map prod $ foldr1 (++) $ map subsequenceForSum [2, 3 ..]