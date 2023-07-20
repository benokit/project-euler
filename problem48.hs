{--
The series, $1^1 + 2^2 + 3^3 + \cdots + 10^{10} = 10405071317$.
Find the last ten digits of the series, $1^1 + 2^2 + 3^3 + \cdots + 1000^{1000}$.
--}

digits :: Integer -> [Integer]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

fromDigits :: [Integer] -> Integer
fromDigits = foldr1 (\d a -> 10 * a + d)

main :: IO()
main = print $ fromDigits $ take 10 $ digits $ sum [n ^ n | n <- [1..1000]]