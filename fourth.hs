minMax :: Int -> Int -> (Int,Int)
minMax a b
    | a>=b      = (b,a)
    | otherwise = (a,b)



primero :: (Int,Int) -> Int
primero(x,y) = x

segundo :: (Num t) => (t,t1) -> t1 --generic
segundo(x,y) = y

suma :: (Int,Int) -> Int
suma a = primero a + segundo a


dosMayores :: Int -> Int -> Int -> (Int,Int)
dosMayores a b c
    | (a>c) && (b>c) = (a,b)
    | (b>a) && (c>a) = (b,c)
    | otherwise      = (a,c)


dosMayores2 :: (Int,Int,Int) -> (Int,Int)
dosMayores2 (a,b,c)
    | (a>c) && (b>c) = (a,b)
    | (b>a) && (c>a) = (b,c)
    | otherwise      = (a,c)


	
