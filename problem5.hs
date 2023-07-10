-- $2520$ is the smallest number that can be divided by each of the numbers from $1$ to $10$ without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from $1$ to $20$?

import Primes (factorize)

compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress (x:xs) = (x, 1 + (length $ takeWhile ((==) x) xs)) : (compress $ dropWhile ((==) x) xs)

combine :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
combine xs [] = xs
combine [] ys = ys
combine xs@((xp,xc):xs') ys@((yp,yc):ys')
    | xp == yp = (xp, max xc yc) : combine xs' ys'
    | xp < yp = (xp, xc) : combine xs' ys
    | otherwise = (yp, yc) : combine xs ys'

evaluate :: [(Int,Int)] -> Int
evaluate [] = 1
evaluate ((xp,xc):xs) = (foldl1 (*) $ take xc $ repeat xp) * (evaluate xs)

main :: IO ()
main = print $ evaluate $ foldl1 combine $ map (compress . factorize) [2,3..20]