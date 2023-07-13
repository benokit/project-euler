{--
$n!$ means $n \times (n - 1) \times \cdots \times 3 \times 2 \times 1$.
For example, $10! = 10 \times 9 \times \cdots \times 3 \times 2 \times 1 = 3628800$,and the sum of the digits in the number $10!$ is $3 + 6 + 2 + 8 + 8 + 0 + 0 = 27$.
Find the sum of the digits in the number $100!$.
--}

fact :: [Integer]
fact = 1 : zipWith (*) fact [2..]

digits :: Integer -> [Integer]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

main :: IO ()
main = print $ sum $ digits $ head $ drop 99 $ fact