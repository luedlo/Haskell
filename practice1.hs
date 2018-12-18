-- 1. Dadas las dimensiones ancho y alto de dos terrenos rectangulares, definir el área del terreno mayor.
terrains :: (Int,Int) -> (Int,Int) -> String
terrains (a,b) (c,d) 
    | (a * b) > (c * d) = "Terrain 1: " ++ show (a * b) ++ "m of area"
    | (c * d) > (a * b) = "Terrain 2: " ++ show (c * d) ++ "m of area"  
    | otherwise         = "Both terrains have the same area!"


{- 2. En el estado de Sinaloa el periodo de siembra de hortalizas es entre los meses de noviembre a enero, fuera de esos periodos la siembra no es exitosa. Definir una función en Haskell que determine si la siembra tendrá éxito teniendo como entrada el mes. El resultado debe ser un valor booleano. -}
successful :: String -> Bool
successful "Noviembre" = True
successful "Diciembre" = True
successful "Enero"     = True
successful anyother    = False


{- 3. Hacer una función que retorne si una persona es niño (de 0 a 11 años), joven (de 12 a 18 años), adulto (de 19 a 50) y adulto mayor (de 50 o más años). -}
toBe :: Int -> String
toBe y
    | y >= 0  && y <= 11 = "You are a Child!"
    | y >= 12 && y <= 18 = "You are a Young!"
    | y >= 19 && y <= 50 = "You are an Adult!"
    | y > 50             = "You are an Ancestor!"
    | otherwise          = "You are Nothing!"


{- 4. Los autos pagan tenencia si son modelo de diez años atrás al año actual. Hacer una función que dado el año actual y el modelo del auto indique si ese auto pagará tenencia o no. -}
tenancy :: Int -> String 
tenancy y 
    | (2018 - y) < 10 = "It do not pay tenancy!"
    | otherwise       = "It pay tenancy!"


{- 5. En el nuevo modelo de competencias los alumnos son calificados como “Sobresalientes“ si obtienen calificación de 95 a 100, “Altamente Competentes” si su calificación es entre 90 y 94, “Competente” si su calificación es 80 a89, “Suficiente” si obtiene entre 70 y 79, en caso que su calificación sea menor a 70 será “No Competente”. Hacer una función en Haskell que reciba el puntaje y retorne como salida la leyenda de competencia correspondiente. -}
qualify :: Int -> String
qualify c
    | c <= 100 && c >= 95 = "Sobresaliente!"
    | c <= 94  && c >= 90 = "Altamente Competente!"
    | c <= 89  && c >= 80 = "Competente!"
    | c <= 79  && c >= 70 = "Suficiente!"
    | c < 70              = "No Competente!"
    | otherwise           = "Try again!"























