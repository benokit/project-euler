{--
The fraction $49/98$ is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that $49/98 = 4/8$, which is correct, is obtained by cancelling the $9$s.
We shall consider fractions like, $30/50 = 3/5$, to be trivial examples.
There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
--}

import Primes (primes)

toNumber :: Int -> Int -> Int
toNumber a b = 10 * a + b

test :: ((Int,Int),Int) -> Bool
test ((a, b), c) = let
    n = toNumber a c
    d = toNumber c b
    in n * b == d * a

reduce :: (Int,Int) -> (Int,Int)
reduce (a, b) = r (a, b) primes
    where
        r (a', b') (p:ps)
            | (p > a' || p > b') = (a', b')
            | (mod a' p) == 0 && (mod b' p == 0) = r (div a' p, div b' p) (p:ps)
            | otherwise = r (a', b') ps

multiply :: (Int,Int) -> (Int,Int) -> (Int,Int)
multiply (a, b) (a', b') = (a * a', b * b')

main :: IO()
main = print $ snd $ reduce $ foldl1 multiply $ map fst $ filter test [((a, b), c) | c <- [1..9], b <- [1..9], a <- [1..(c - 1)]]