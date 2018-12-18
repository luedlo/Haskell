-- Fichas Domino
fichas = [(x,y)|x<-[0..6], y<-[x..6]]


-- Factor of n number
factor :: Int -> [Int]
factor n = [x| x<-[1..n], mod n x == 0]


-- Cousin numbers
cousin :: Int -> Bool
cousin n = factor n == [1,n]


-- List of Cousin numbers
cousinL :: Int -> [Int]
cousinL n = [x| x<-[2..n], factor x == [1,x]] {- cousin x -}


-- zip function
list = zip [1..1000] [1..100]


-- Functions of Haskell to obtain values of the list: head, tail, init, last


--
pairs :: [a] -> [(a,a)]
pairs 






