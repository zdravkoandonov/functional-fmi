symNumber n = helper 0 n where
	helper curRev curN
		|curN == 0 = curRev
		|otherwise = helper (curRev * 10 + curN `mod` 10) (curN `div` 10)

isPalindrome n = (n == symNumber n)

frequencyDictionary [] = []
frequencyDictionary l = qsort (map (\ (x) -> (x:[occurrenceCount x l])) (uniq l)) where
	occurrenceCount _ [] = 0
	occurrenceCount x (y:ys) = (if x == y then 1 else 0) + occurrenceCount x ys
	uniq [] = []
	uniq (x:xs) = if elem x xs then uniq xs else x:uniq xs
	qsort [] = []
	qsort (x:xs) = (qsort (filter (\ (z:zs) -> head zs > head (tail x)) xs)) ++ [x] ++ (qsort (filter (\ (z:zs) -> head zs <= head (tail x)) xs))

frequencyDictionary' [] _ = []
frequencyDictionary' l1 l2 = qsort (map (\ (x) -> [x, occurrenceCount x l2]) (uniq l1)) where
	occurrenceCount	_ [] = 0
	occurrenceCount x (y:ys) = (if x == y then 1 else 0) + occurrenceCount x ys
	uniq [] = []
	uniq (x:xs) = if elem x xs then uniq xs else x:(uniq xs)
	qsort [] = []
	qsort (x:xs) = qsort [y | y <- xs, last y > last x] ++ [x] ++ qsort [y | y <- xs, last y <= last x]

variations 0 _ = [[]]
variations k l = foldl1 (++) (map (\ (x) -> (map (\ (y) -> (x:y)) (variations (k - 1) [y | y <- l, x /= y]))) l)

variations' k l p q = [x | x <- variations k l, (foldr1 (-) x) `elem` [p..q]]

variations'' k l p q = [x | x <- variations k l, (foldl1 (+) x) `elem` [p..q]]

merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

mergeMany l = foldl merge [] l

sampleInfiniteStream = helper 1 5 where
	helper a b = [a..b] : helper (a + 1) (b + 1)

maxOfStreamLists (x:xs) = (foldl1 max x) : maxOfStreamLists xs

isPrime n = length [x | x <- [1..(n - 1)], n `mod` x == 0] == 1

primesOfStreamLists (x:xs) = (filter isPrime x) : primesOfStreamLists xs

insertionSort l = helper [] l where
	helper sorted [] = sorted
	helper sorted (x:xs) = helper ((filter (<= x) sorted) ++ [x] ++ (filter (> x) sorted)) xs

isPerfect n = foldl (+) 0 [x | x <- [1..(n-1)], n `mod` x == 0] == n

perfectsOfStreamLists (x:xs) = (filter isPerfect x) : perfectsOfStreamLists xs

fibs = helper 0 1 where
	helper prev cur = cur : (helper cur (prev + cur))

fibsConcat = helper (tail fibs) (head fibs) where
	numLen n
		| n < 10 = 1
		| otherwise = 1 + numLen (n `div` 10)
	helper (x:xs) n = n : helper xs (n * (10 ^ (numLen x)) + x)

oddFibs' = filter odd fibs

oddFibs = helper 0 1 where
	helper x y = if odd y then y : helper y (x + y) else helper y (x + y)

oddFibsConcat = helper (tail oddFibs) (head oddFibs) where
	helper (x:xs) curN = curN : helper xs (curN * (10 ^ numLen x) + x)
	numLen n = if n < 10 then 1 else 1 + numLen (n `div` 10)