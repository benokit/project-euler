{--
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
--}

import Data.Numbers.Primes (primes, isPrime)

masks :: Int -> Int -> [[Bool]]
masks m n
    | m == 0 = []
    | n == 0 = []
    | n > m = []
    | n == m = [take m $ repeat True]
    | otherwise = (map ((:) True) $ masks (m - 1) (n - 1)) ++ (map ((:) False) $ masks (m - 1) n)

areEqual :: [Int] -> Bool
areEqual [] = True
areEqual (x:xs) = foldr1 (&&) $ map (==x) xs

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

takeWithMask :: [Bool] -> [a] -> [a]
takeWithMask [] _ = []
takeWithMask _ [] = []
takeWithMask (b:bs) (x:xs)
    | b = x : takeWithMask bs xs
    | otherwise = takeWithMask bs xs

setDigits :: [Bool] -> [Int] -> Int -> [Int]
setDigits [] ds _ = ds
setDigits (b:bs) (d:ds) d' = (if b then d' else d) : setDigits bs ds d'

testForMask :: [Int] -> (Int,[Bool]) -> Bool
testForMask ds (n,bs) = 
    let 
        ds' = takeWithMask bs ds
        d' = head ds'
    in (length ds' == n) && (d' < 3) && (all (==d') ds') && ((>6) $ length $ filter isPrime $ map (fromDigits . setDigits bs ds) [(d' + 1)..9])

test :: Int -> Bool
test p =
    let
        ds = digits p
        l = length ds
    in foldr1 (||) $ map (testForMask ds) $ foldr1 (++) $ [map (\m -> (i, m)) $ masks (l - 1) i | i <- [2..(l - 1)]]

main :: IO()
main = print $ head $ filter test $ dropWhile (\p -> 0 == div p 10000) primes