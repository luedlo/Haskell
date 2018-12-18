-- Distance between points
distance :: (Int,Int) -> (Int,Int) -> Int
distance (x1,y1) (x2,y2) = abs((x2-x1) + (y2-y1))

distance2 :: (Int,Int) -> (Int,Int) -> Int
distance2 (x1,y1) (x2,y2) = answer
 where 
    answer = abs((x2-x1) + (y2-y1))
 
 
-- Middle Point between coordinates
middlePoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
middlePoint (x1,y1) (x2,y2) = (a,b)
 where 
    a = (x1+x2)/2
    b = (y1+y2)/2


-- Lists
list = [1..10]

list2 = [x^2 | x<-[1..10]]

list3 = [(x,y) | x<-[1..3], y<-[4,5]]

list4 = [(a,b,c)| a<-[1..100] , b<-[1..100] , c<-[1..100] , c^2==((a^2)+(b^2))]


















