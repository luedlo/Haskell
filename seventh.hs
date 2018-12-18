-- Find any times y positions that x num was found in an array 
position :: Eq a=>a -> [a] -> [Int] 
position x xs =
    [i | (x',i) <- zip xs [0..], x == x']

-- Length of a String 
l = length "abcde"

-- Take the three first Char
t = take 3 "abcde"

-- Combine a String w/ an Array until the length of the most short
z = zip "abc" [1,2,3,4]

c :: Char -> String -> Int 
c x xs = length [x' | x' <- xs, x==x']

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x<-[1..n]]