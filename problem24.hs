{--
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
012   021   102   120   201   210
What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
--}

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations xs = f [] xs
    where
        f ys [] = []
        f ys' (y:ys) = (g y (ys' ++ ys)) ++ (f (ys' ++ [y]) ys)
        g y ys = map ((:) y) $ permutations ys

main :: IO()
main = print $ head $ drop 999999 $ permutations [0..9]