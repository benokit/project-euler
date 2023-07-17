{--
A unit fraction contains $1$ in the numerator. The decimal representation of the unit fractions with denominators $2$ to $10$ are given:
Where $0.1(6)$ means $0.166666\cdots$, and has a $1$-digit recurring cycle. It can be seen that $1/7$ has a $6$-digit recurring cycle.
Find the value of $d \lt 1000$ for which $1/d$ contains the longest recurring cycle in its decimal fraction part.
--}

import Data.Map (empty, insert, notMember, (!))

rcl :: Int -> Int
rcl n = (\(r, i, m) -> i - (m ! r)) $ head $ dropWhile (\(r, _, m) -> notMember r m) $ iterate (\(r, i, m) -> let r' = mod (10 * r) n in (r', i + 1, insert r i m)) (1, 1, empty)

main :: IO()
main = print $ fst $ foldr1 (\(i,c) (i',c') -> if c > c' then (i, c) else (i', c')) $ take 999 $ [(i, rcl i) | i <- [1..]]