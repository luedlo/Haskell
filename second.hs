{- Multiple Comments -}
-- Simple Comments --

data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
 deriving (Eq, Ord, Enum, Read, Show, Bounded) 

diaLaborable :: DiaSemana -> Bool
diaLaborable d = Lunes <= d && Viernes >= d

diaFestivo :: DiaSemana -> Bool
diaFestivo d = d==Sabado || d==Domingo


--

maxMine :: Int -> Int -> Int -> Int
maxMine a b c
 | a >= b && a >= c = a
 | b >= c           = b
 | otherwise        = c

 
----------------------------------------

data Estado = Bajo | Normal | SobrePeso | Obesidad
 deriving Show
 
imc :: (RealFloat a) => a -> Estado
imc miIMC
 | miIMC <= 16 = Bajo
 | miIMC <= 24 = Normal
 | miIMC < 30  = SobrePeso
 | otherwise = Obesidad
 
----------------------------------------

numero :: Int -> Int
numero num
 | num <= 10 = num
 | num <= 100 = num * 2
 | otherwise = num * 3
 
 
-----------------------------------------
data Paga = PagoNormal | PagoDoble
 deriving Show

horas :: Int -> String
horas h
 | h <= 8 = "PagoNormal"
 | otherwise  = "PagoDoble"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 