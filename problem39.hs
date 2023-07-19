{--
If $p$ is the perimeter of a right angle triangle with integral length sides, $\{a, b, c\}$, there are exactly three solutions for $p = 120$.
$\{20,48,52\}$, $\{24,45,51\}$, $\{30,40,50\}$
For which value of $p \le 1000$, is the number of solutions maximised?
--}

import Data.List (maximumBy)
import Data.Ord (comparing)

test :: Int -> (Int, Int) -> Bool
test p (a, b) = let c = p - a - b in a * a + b * b == c * c

triangleCount :: Int -> Int
triangleCount p = length $ filter (test p) [(a, b) | a <- [1..(p - 1)], b <- [a..(p - 1)], a + b < p && p - (a + b) > a && p - (a + b) > b]

main :: IO()
main = print $ fst $ maximumBy (comparing snd) $ map (\n -> (n, triangleCount n)) [1..1000]