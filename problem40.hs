{--
An irrational decimal fraction is created by concatenating the positive integers:
$$0.12345678910{\color{red}\mathbf 1}112131415161718192021\cdots$$
It can be seen that the $12$th digit of the fractional part is $1$.
If $d_n$ represents the $n$th digit of the fractional part, find the value of the following expression.
$$d_1 \times d_{10} \times d_{100} \times d_{1000} \times d_{10000} \times d_{100000} \times d_{1000000}$$
--}

digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

ds :: [Int]
ds = foldr1 (++) $ map digits [1..]

main :: IO()
main = print $ foldl1 (*) $ map (\n -> ds !! (n - 1)) [1,10,100,1000,10000,100000,1000000]