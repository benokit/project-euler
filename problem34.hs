{--
$145$ is a curious number, as $1! + 4! + 5! = 1 + 24 + 120 = 145$.
Find the sum of all numbers which are equal to the sum of the factorial of their digits.
Note: As $1! = 1$ and $2! = 2$ are not sums they are not included.
--}

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact 3 = 6
fact 4 = 24
fact 5 = 120
fact 6 = 720
fact 7 = 5040
fact 8 = 40320
fact 9 = 362880

maxNumber :: Int
maxNumber = snd $ head $ dropWhile (\(p, n) -> p * (fact 9) > n) $ map (\p -> (p, 10^p)) [2..]

digits :: Int -> [Int]
digits 0 = []
digits n = (mod n 10) : digits (div n 10)

test :: Int -> Bool
test n = n == (sum $ map fact $ digits n)

main :: IO()
main = print $ sum $ filter test [10..maxNumber]