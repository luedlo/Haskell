-- Example of sum
size :: Int
size = 12+13

-- Example of Number Square
square :: Int -> Int
square n = n*n

-- The only time 'if' that I will use it in this course
chooseGreeting x = if x then "GoodBye." else "Hello!"

threeEquals :: Int -> Int -> Int -> Bool
threeEquals m n p = (m==n) && (n==p)

--
data Temp = Calor | Frio | Templado
 deriving Show
data Estacion = Primavera | Otoño | Verano | Invierno

tempNormal :: Estacion -> Temp
tempNormal Verano = Calor
tempNormal Invierno = Frio
tempNormal _ = Templado