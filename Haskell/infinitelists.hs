import Data.List

naturals = helper 1 where
	helper x = x : helper (x + 1)

points (x, y) = (x, y) : points (x+1, y+1)

filterPoints points = filter (\ (x, y) -> x == y) points

distances points = map (\ (x, y) -> sqrt (x ** 2 + y ** 2)) points

tribonacci = 1:1:1:helper 1 1 1 where
	helper x y z = (x + y + z) : helper y z (x + y + z)

sieve = helper [2..] where
	helper (x:xs) = x : helper [y | y <- xs, y `mod` x > 0]

qsort [] = []
qsort (x:xs) = qsort (filter (\ (y) -> y <= x) xs) ++ [x] ++ qsort (filter (\ (y) -> y > x) xs)

arrmin l = foldl1 min l

selsort [] = []
selsort l = minEl:selsort (delete minEl l) where
	minEl = arrmin l