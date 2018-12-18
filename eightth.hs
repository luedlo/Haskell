-- Product of n numbers in a list
producto :: Num a => [a] -> a
producto [] = 1
producto (n:ns) = n * producto ns

-- Reverse the list
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


-- Incomplete
zip2 :: [a] -> [b] -> [(a,b)]


-- Remove the first n elements from a list
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs


-- Appending two list
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)


-- QuickSort
qsort :: ORd a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
		larger =  [b | b <- xs, b > x]










