{--
The decimal number, $585 = 1001001001_2$ (binary), is palindromic in both bases.
Find the sum of all numbers, less than one million, which are palindromic in base $10$ and base $2$.
(Please note that the palindromic number, in either base, may not include leading zeros.)
--}

ps :: Int -> [[Int]]
ps 0 = [[]]
ps 1 = [[n] | n <- [0..9]]
ps i = [[n] ++ p ++ [n] | n <- [0..9], p <- ps (i - 2)]

fromDigits :: [Int] -> Int
fromDigits = foldl (\a d -> 10 * a + d) 0

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary x = mod x 2 : toBinary (div x 2)

isBinaryPalindrome :: Int -> Bool
isBinaryPalindrome x = let b = toBinary x in foldr1 (&&) $ zipWith (==) b (reverse b)

main :: IO()
main = print $ sum $ filter isBinaryPalindrome $ map fromDigits $ filter ((/= 0) . (`mod` 2) . head) $ filter ((/= 0)  . head) $ foldr1 (++) $ map ps [1..6]