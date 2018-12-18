-- Proyecto c45 (100) :v
-- Aula BD 9-10 am 
-- Integrantes:
--    Caro Zavala Angilberto
--    Lopez Vega Luis Eduardo
--    Velarde Ramirez Gerardo German


-- Listas de Clases
age = ["Young","Young","Young","Young","Young","Middle","Middle","Middle","Middle","Middle","Old","Old","Old","Old","Old"]

has_job = ["False","False","True","True","False","False","False","True","False","False","False","False","True","True","False"]

own_house = ["False","False","False","True","False","False","False","True","True","True","True","True","False","False","False"]

credit_rating = ["Fair","Good","Good","Fair","Fair","Fair","Good","Good","Exelle.","Exelle.","Exelle.","Good","Good","Exelle.","Fair"]

clas = ["No","No","Yes","Yes","No","No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No"]

-- Lista de las clases
clases = [age,has_job,own_house,credit_rating]

age' = ["Young","Young","Young","Young","Middle","Middle","Old","Old","Old"]

has_job' = ["False","False","True","False","False","False","True","True","False"]

credit_rating' = ["Fair","Good","Good","Fair","Fair","Good","Good","Exelle.","Fair"]

clas' = ["No","No","Yes","No","No","No","Yes","Yes","No"]

clasesOH = [age',has_job',credit_rating',clas']


-- Auxiliar de Entropia Global
yes :: [String] -> Float
yes lista = o lista "Yes"

no :: [String] -> Float
no lista = o lista "No"


-- Entropia Global
entropiaGlobal :: [String] -> Float 
entropiaGlobal lista = (-(no lista / tamaño lista)) * (log (no lista / tamaño lista) / log 2) - (yes lista / tamaño lista) * (log (yes lista / tamaño lista) / log 2)


-- dj/D
dj_D :: Float -> Float
dj_D n = n / tamaño clas

-- Devuelve Cuantos No/Yes hay de un Tipo
noORyes :: [String] -> [String] -> String -> String -> Float-> Float
noORyes [] [] _ _ e = e
noORyes (x:xs) (y:ys) c d e
        | x == c && y == d = noORyes xs ys c d (e + 1)
        | otherwise        = noORyes xs ys c d e


-- Forma Acortada de noORyes
nOy :: [String] -> String -> String -> Float
nOy a b c = noORyes a clas b  c 0 


-- Entropy(Dj)
entropiaDj :: [String] -> String -> Float
entropiaDj lista tipo 
    | isNaN ((-(nOy lista tipo "No" / o lista tipo)) * (log (nOy lista tipo "No" / o lista tipo) / log 2) - (nOy lista tipo  "Yes"/o lista tipo) * (log (nOy lista tipo "Yes"/o lista tipo) / log 2)) = 0
    | otherwise = ((-(nOy lista tipo "No" / o lista tipo)) * (log (nOy lista tipo "No" / o lista tipo) / log 2) - (nOy lista tipo  "Yes"/o lista tipo) * (log (nOy lista tipo "Yes"/o lista tipo) / log 2))


-- (dj/D) * (Entropy(Dj))
dXe :: Float -> Float -> Float
dXe a b 
    | isNaN (a * b) = 0
    | otherwise = dj_D a * b


-- P()
p :: [String] -> String -> [Float]
p lista tipo = [dj_D (o lista tipo), entropiaDj lista tipo, dXe (o lista tipo) (entropiaDj lista tipo)]


-- Encabezados de Entropia por Clase
encabezados :: [[String]]
encabezados = [["P(Age)    ","dj/D","", "Entropy(Dj)"],["P(Has_Job)","dj/D","", "Entropy(Dj)"],["P(Own_H)","dj/D","", "Entropy(Dj)"],["P(Credit)","dj/D","", "Entropy(Dj)"]]


-- Selecciona un encabezado
encabezado :: [[String]] -> Int -> [String]
encabezado (x:xs) n
    | n == 0    = x
    | otherwise = encabezado xs (n - 1)


-- Forma acortada de encabezado
e :: Int -> [String]
e n = encabezado encabezados n


-- Muestra el encabezado
etiqueta :: [String] -> IO()
etiqueta [] = putStrLn ""
etiqueta (x:xs) = do 
    putStr (x ++ "\t")
    etiqueta xs


-- Muestra la info numerica
mostrar :: [Float] -> IO()
mostrar [] = putStrLn ""
mostrar (x:xs) = do 
    putStr (show x ++ "\t")
    mostrar xs


-- Toma el Valor de Entropia por Tipo
entropiaXtipo :: [Float] -> Float
entropiaXtipo a = last a    

-----------------------------------------------------------
-- Determina la Entropia por Tipo
entropiaPORtipo :: [String] -> [String] -> Float -> Float
entropiaPORtipo _ [] suma = suma 
entropiaPORtipo lista (x:xs) suma = entropiaPORtipo lista xs (suma + entropiaXtipo (p lista x))


-- Forma Acortada de entropiaPORtipo
eXt :: [String] -> [String] -> Float
eXt lista tipo = entropiaPORtipo lista tipo 0 


-- Auxiliar de verEntropia
ve :: [String] -> Float
ve lista = eXt lista (ele lista) 


-- Muestra la Entropia por Clase
verEntropia :: [String] -> [String] -> IO()
verEntropia [] _ = putStr ""
verEntropia (x:xs) lista = do
    putStrLn (x ++ " = " ++ show (ve lista) ++ "\n")
    verEntropia [] lista

--------------------------------------------------------------
-- Determina cuantas entropias por tipo hay que hacer
entropia :: [String] -> [String] -> IO()
entropia lista [] = putStr ""
entropia lista (x:xs) = do
    verElemento x
    tabulador 1
    mostrar (p lista x)
    entropia lista xs


-- No se que es el Gain pero aqui esta :V
gainD :: Float -> Float -> Float
gainD a b = a - b 


-- Muestra los Tipos
verElemento :: String -> IO() 
verElemento x = do 
    putStr (x ++ "\t")


-- Determina Cuantas Clases hay que Hacer
hacerClases :: [[String]] -> Int -> IO()
hacerClases [] _ = putStr ""
hacerClases (x:xs) n = do
    etiqueta (e n)
    entropia x (ele x)
    verEntropia (e n) x
    hacerClases xs (n + 1)


-- Auxiliar del Gain
gain :: [String] -> IO()
gain x = putStrLn (show (gainD (entropiaGlobal clas) (ve x)))


-- Determina Cuantos Gain hay que Hacer
hacerGain :: [[String]] -> Int -> IO()
hacerGain [] _ = putStr ""
hacerGain (x:xs) n = do
    putStr ("Gain " ++ head (e n) ++ "\t\t")
    gain x
    hacerGain xs (n + 1)


-- Inician los Calculos del c45
inicio = do
    putStr ("\nEntropia Global: " ++ show (entropiaGlobal clas) ++ "\n")
    putStrLn "\n----------------------------------------------------------\n---------------------- Iteracion 0 -----------------------\n----------------------------------------------------------\n"
    hacerClases clases 0
    putStrLn ""
    hacerGain clases 0

    putStrLn "\n----------------------------------------------------------\n---------------------- Iteracion 1 -----------------------\n----------------------------------------------------------\n"
    hacerClases clases 0
    putStrLn ""
    hacerGain clases 0




---------------------------------------------------------
--------------------- Utilerias -------------------------
---------------------------------------------------------

-- Devuelve el tamaño de un arreglo
tamaño xs = sum [1 | _ <- xs]


-- Imprime n Tabuladores
tabulador :: Int -> IO()
tabulador 0 = putStr ""
tabulador n  
    | n > 0 = do putStr ("\t") 
                 tabulador (n - 1)


-- Devuelve el numero de ocurrencias de un elemento "n"
ocurrencias :: [String] -> String -> Float -> Float
ocurrencias [] b n = n
ocurrencias (x:xs) b n = go b n
    where
    go b n 
        | x == b    = ocurrencias xs b (n + 1)
        | otherwise = ocurrencias xs b n


-- Forma Acortada de Ocurrencias
o :: [String] -> String -> Float
o a b = ocurrencias a b 0


-- Devuelve los Elementos Diferentes de una Lista
elementos :: [String] -> [String] -> [String]
elementos [] b = b
elementos (x:xs) b
        | elem x b == False = elementos xs (b++[x])
        | otherwise         = elementos xs b


-- Forma Acortada de Elementos
ele :: [String] -> [String]
ele lista = elementos lista []


-- Muestra todas las clases
t :: IO()
t = do
    putStrLn ("\nAge\t\tHas_Job\t\tOwn_House\tCredit_Rating\tclas")
    putStrLn "--------------------------------------------------------------------"
    tablas age has_job own_house credit_rating clas


-- Verificacion de Tablas
tablas :: [String] -> [String] -> [String] -> [String] -> [String] -> IO()
tablas [] _ _ _ _ = putStr ""
tablas (x1:xs1) (x2:xs2) (x3:xs3) (x4:xs4) (x5:xs5) = do
    putStr (x1 ++ "\t\t")
    putStr (x2 ++ "\t\t")
    putStr (x3 ++ "\t\t")
    putStr (x4 ++ "\t\t")
    putStr (x5 ++ "\n")
    tablas xs1 xs2 xs3 xs4 xs5


-- Codigo sucio por que es estatico en esta parte (aunque solo son etiquetas)

-- Determina Cuantas Clases hay que Hacer
hacerClases' :: [[String]] -> Int -> IO()
hacerClases' [] _ = putStr ""
hacerClases' (x:xs) n = do
    etiqueta (e' n)
    entropia x (ele x)
    verEntropia (e' n) x
    hacerClases' xs (n + 1)

encabezadosOH :: [[String]]
encabezadosOH = [["P(Age)    ","dj/D","", "Entropy(Dj)"],["P(Has_Job)","dj/D","", "Entropy(Dj)"],["P(Credit)","dj/D","", "Entropy(Dj)"]]

e' :: Int -> [String]
e' n = encabezado encabezadosOH n

etiqueta' :: [String] -> IO()
etiqueta' [] = putStrLn ""
etiqueta' (x:xs) = do 
    putStr (x ++ "\t")
    etiqueta' xs


-- Intentos fallidos :(
{-
si :: String -> [String] -> Int -> Bool
si a (y:ys) n = si a ys (n - 1)
    where 
    go a
        | y == a = True 


p' :: [String] -> String -> Int -> [Float]
p' lista tipo n
    | si "False" own_house n == True = [dj_D (o lista tipo), entropiaDj lista tipo, dXe (o lista tipo) (entropiaDj lista tipo)]
	| otherwise = []

entropiaPORtipo' :: [String] -> [String] -> Float -> Int -> Float
entropiaPORtipo' _ [] suma n = suma 
entropiaPORtipo' lista (x:xs) suma n = entropiaPORtipo' lista xs (suma + entropiaXtipo (p lista x 0)) n


entropia' :: [String] -> [String] -> Int -> IO()
entropia' lista [] = putStr ""
entropia' lista (x:xs) = do
    verElemento x
    tabulador 1
    mostrar (p' lista x 0)
    entropia' lista xs
-}











