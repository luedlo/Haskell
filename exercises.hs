-- Definir una función que nos diga qué meses corresponden  a cada estación
-- ejemplo: Primavera == Marzo, Abril, Mayo
season :: String -> String
season "Primavera" = "Marzo, Abril, Mayo"
season "Verano"    = "Junio, Julio, Agosto"
season "Otoño"     = "Septiembre, Octubre, Noviembre"
season "Invierno"  = "Diciembre, Enero, Febrero"


--Definir la función mcd, tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el algoritmo de Euclides. Por ejemplo, mcd 30 45 == 15
mcd :: Integer -> Integer -> Integer
mcd a b
    | mod a b == 0 = b
    | otherwise    = mcd b (mod a b)


-- Calcula el factorial de un numero usando: a) guardas y b) Patrones
factA :: Int -> Int
factA n 
    | n == 0 = 1
    | otherwise = n * factA (n-1)


factB :: Int -> Int
factB 0 = 1
factB n = n * factB (n-1)


-- Decir hacia donde miras si giras 90 grados
data Direccion = Norte | Sur | Este | Oeste
 deriving Show

girar90 :: Direccion -> Direccion
girar90 Norte = Oeste
girar90 Sur = Este
girar90 Este = Norte
girar90 Oeste = Sur


-- Decir que color sigue en el semaforo a partir del color actual
data Color = Verde | Amarillo | Rojo
 deriving Show

semaforo :: Color -> Color
semaforo Verde    = Amarillo
semaforo Amarillo = Rojo
semaforo Rojo     = Verde


-- Dice si el parametro b es multiplo del parametro a
multiploDe :: Int -> Int -> Bool 
multiploDe a b 
    | mod a b == 0 = True
    | otherwise    = False


{- No entendi el problema :c

-- Retorna las raíces de una funcion en base a sus coeficientes
raíces :: Float ->    Float -> Float -> (Float, Float)

-}


-- Funcion que reciba un numero y retorne cero
cero :: Integer -> Integer
cero(n) = 0


-- Recibe la edad de una persona y le dice si puede votar
votar :: Int -> Bool
votar e 
    | e >= 18   = True
    | otherwise = False


-- Dice si se presenta examen, estan excentos solo los 100 
--excenta :: (Eq a, Num a) => a -> [Char] ------------------   esta parte :v
excenta :: Int -> String
excenta c
	| c == 100  = "Excento de Examen!"
    | otherwise = "Presentas examen!"






