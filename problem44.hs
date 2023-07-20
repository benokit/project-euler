{--
<p>Pentagonal numbers are generated by the formula, $P_n=n(3n-1)/2$. The first ten pentagonal numbers are:
$$1, 5, 12, 22, 35, 51, 70, 92, 117, 145, \dots$$</p>
<p>It can be seen that $P_4 + P_7 = 22 + 70 = 92 = P_8$. However, their difference, $70 - 22 = 48$, is not pentagonal.</p>
<p>Find the pair of pentagonal numbers, $P_j$ and $P_k$, for which their sum and difference are pentagonal and $D = |P_k - P_j|$ is minimised; what is the value of $D$?</p>
--}

import Data.Map (Map, fromList, member, notMember)
import Data.Maybe (catMaybes)

ps :: [Int]
ps = 1 : zipWith (+) ps [4,7..]

mps :: Map Int Bool
mps = fromList [(p, True) | p <- take 10000 ps]

test :: (Int, Int) -> Maybe Int
test (a, b) 
    | notMember (a + b) mps = Nothing
    | member (2 * a + b) mps = Just b
    | member (2 * b + a) mps = Just a
    | otherwise = Nothing

main :: IO()
main = print $ head $ catMaybes $ map test [(a, b) | b <- ps, a <- (takeWhile (<b) ps)]