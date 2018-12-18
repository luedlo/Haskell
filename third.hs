-- Conditional Expressions

absolute :: Int -> Int
absolute n | n >= 0    = n
           | otherwise = -n


-- Signum of number value
sign :: Int -> Int
sign n | n < 0     = -1
       | n == 0    = 0
       | otherwise = 1

   
-- Boolean compare
andF :: Bool -> Bool -> Bool
andF a b | a && b    = True
         | otherwise = False


orF :: Bool -> Bool -> Bool
orF a b | not(a && b)= False
        | otherwise  = True