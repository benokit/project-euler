{--
We shall say that an $n$-digit number is pandigital if it makes use of all the digits $1$ to $n$ exactly once; for example, the $5$-digit number, $15234$, is $1$ through $5$ pandigital.
The product $7254$ is unusual, as the identity, $39 \times 186 = 7254$, containing multiplicand, multiplier, and product is $1$ through $9$ pandigital.
Find the sum of all products whose multiplicand/multiplier/product identity can be written as a $1$ through $9$ pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
--}

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = f [] xs
    where
        f ys [] = []
        f ys' (y:ys) = (g y (ys' ++ ys)) ++ (f (ys' ++ [y]) ys)
        g y ys = map ((:) y) $ permutations ys

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = (sort $ filter (<x) xs) ++ [x] ++ (sort $ filter (>=x) xs)

rmDup :: Eq a => [a] -> [a]
rmDup [] = []
rmDup (x:xs) = x : (rmDup $ dropWhile (==x) xs)

partition :: [Int] -> (Int, Int) -> (Int, Int, Int)
partition xs (a,b) = (fromDigits $ take a xs, fromDigits $ take b (drop a xs), fromDigits $ drop (a + b) xs)

partitions :: [Int] -> [(Int,Int,Int)]
partitions x = map (partition x) [(1, 4), (2, 3)]

isProduct :: (Int, Int, Int) -> Bool
isProduct (a, b, c) = a * b == c

main :: IO()
main = print $ sum $ rmDup $ sort $ map (\(_,_,p) -> p) $ filter isProduct $ foldr1 (++) $ map partitions $ permutations [1..9]