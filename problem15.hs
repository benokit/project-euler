-- Starting in the top left corner of a $2 \times 2$ grid, and only being able to move to the right and down, 
-- there are exactly $6$ routes to the bottom right corner.
-- How many such routes are there through a $20 \times 20$ grid?

nPaths :: (Int,Int) -> Int
nPaths (0,_) = 1
nPaths (_,0) = 1
nPaths (n,m) = [[f (i, j) | i <- [1..]] | j <- [1..]] !! (n - 1) !! (m - 1)
    where f (i, j) = nPaths (i - 1, j) + nPaths (j - 1, i)

main :: IO ()
main = print $ nPaths (20,20)