{--
Take the number $192$ and multiply it by each of $1$, $2$, and $3$:
\begin{align}
192 \times 1 &amp;= 192\\
192 \times 2 &amp;= 384\\
192 \times 3 &amp;= 576
\end{align}
By concatenating each product we get the $1$ to $9$ pandigital, $192384576$. We will call $192384576$ the concatenated product of $192$ and $(1,2,3)$.
The same can be achieved by starting with $9$ and multiplying by $1$, $2$, $3$, $4$, and $5$, giving the pandigital, $918273645$, which is the concatenated product of $9$ and $(1,2,3,4,5)$.
What is the largest $1$ to $9$ pandigital $9$-digit number that can be formed as the concatenated product of an integer with $(1,2, \dots, n)$ where $n \gt 1$?
--}

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = f [] xs
    where
        f ys [] = []
        f ys' (y:ys) = (g y (ys' ++ ys)) ++ (f (ys' ++ [y]) ys)
        g y ys = map ((:) y) $ permutations ys


test :: [Int] -> Bool
test ds = foldr1 (||) $ map g [1..4]
    where 
        g k = let n = fromDigits $ take k ds in q n 2 (drop k ds)
        q _ _ [] = True
        q n m ds' = let ds'' = digits (m * n) in (p ds' ds'') && (q n (m + 1) (drop (length ds'') ds'))
        p [] [] = True
        p _ [] = False
        p (d':ds') (d'':ds'') = (d' == d'') && p ds' ds''


main :: IO()
main = print $ fromDigits $ head $ filter test $ permutations [9,8..1]