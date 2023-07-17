{--
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
\begin{align}
1634 &amp;= 1^4 + 6^4 + 3^4 + 4^4\\
8208 &amp;= 8^4 + 2^4 + 0^4 + 8^4\\
9474 &amp;= 9^4 + 4^4 + 7^4 + 4^4
\end{align}
As $1 = 1^4$ is not a sum it is not included.
The sum of these numbers is $1634 + 8208 + 9474 = 19316$.
Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--}

p5 :: Int -> Int
p5 0 = 0
p5 1 = 1
p5 2 = 32
p5 3 = 243
p5 4 = 1024
p5 5 = 3125
p5 6 = 7776
p5 7 = 16807
p5 8 = 32768
p5 9 = 59049

digits :: Int -> [Int]
digits 0 = []
digits n = (mod n 10) : digits (div n 10)

sp5 :: Int -> Int
sp5 = sum . map p5 . digits

main :: IO()
main = print $ sum $ filter (\n -> n == sp5 n) [2..9^6]