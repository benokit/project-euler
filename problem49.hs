{--
The arithmetic sequence, $1487, 4817, 8147$, in which each of the terms increases by $3330$, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the $4$-digit numbers are permutations of one another.
There are no arithmetic sequences made up of three $1$-, $2$-, or $3$-digit primes, exhibiting this property, but there is one other $4$-digit increasing sequence.
What $12$-digit number do you form by concatenating the three terms in this sequence?
--}

import Data.Numbers.Primes (primes, isPrime)

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = f [] xs
    where
        f ys [] = []
        f ys' (y:ys) = (g y (ys' ++ ys)) ++ (f (ys' ++ [y]) ys)
        g y ys = map ((:) y) $ permutations ys

atLestNumberOfElements :: Int -> [a] -> Bool
atLestNumberOfElements 0 _ = True
atLestNumberOfElements _ [] = False
atLestNumberOfElements n (x:xs) = atLestNumberOfElements (n - 1) xs

partitionInto :: Int -> [Int] -> [[Int]]
partitionInto 0 _ = []
partitionInto 1 xs = [[x] | x <- xs]
partitionInto n xs
    | atLestNumberOfElements n xs = let x = head xs in (map ((:) x) $ partitionInto (n - 1) (tail xs)) ++ (partitionInto n (tail xs))
    | otherwise = []

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

areEquidistant :: [Int] -> Bool
areEquidistant xs = let (d:ds) = zipWith (-) (tail xs) xs in (d > 0) && (foldr1 (&&) $ map (==d) ds) 

getTriplets :: Int -> [[Int]]
getTriplets p = filter areEquidistant $ partitionInto 3 $ filter isPrime $ map fromDigits $ permutations $ digits p

main :: IO()
main = print $ present $ head $ filter (\(p:_) -> p /= 1487) $ foldr1 (++) $ map getTriplets $ takeWhile (<10000) $ dropWhile (<1000) primes
    where present = fromDigits . foldr1 (++) . map digits 