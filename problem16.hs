-- $2^{15} = 32768$ and the sum of its digits is $3 + 2 + 7 + 6 + 8 = 26$.
-- What is the sum of the digits of the number $2^{1000}$?

pow2 :: Int -> Integer
pow2 0 = 1
pow2 1 = 2
pow2 n = let 
    q = div n 2
    p = n - q
    z = pow2 q in if p == q then z * z else z * (pow2 p)

digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

main :: IO ()
main = print $ sum $ digits $ pow2 1000