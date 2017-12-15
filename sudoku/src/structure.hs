import Data.Char
import Data.List
import System.IO
import Data.Maybe

-- Toma el tablero como viene, y coloca ceros en los espacios vacíos
primeraPasada :: [Char] -> [Int]
primeraPasada [] = []
primeraPasada (a:as)
					| a == '-' = 0 : (primeraPasada as) 
					|otherwise = digitToInt(a) : (primeraPasada as)

-- Funcion que imprime el tablero en pantalla
-- puede ser mejorada con las funciones de Data.List
printAsMatriz :: [Int] -> String
printAsMatriz [] = " ________"
printAsMatriz (a:b:c:d:as) = " ________" ++ '\n' : '|' : (intToDigit a) : '|': (intToDigit b) : '|': (intToDigit c) : '|': (intToDigit d) : '|' :'\n' : [] ++ printAsMatriz as

-- Funcion que separa el tablero en una lista de filas
toFilas :: [a] -> [[a]]
toFilas [] = []
toFilas t = take 4 t : toFilas (drop 4 t) 

-- Funcion que separa el tablero en una lista de columnas
toColumnas :: [[Int]] -> [[Int]]
toColumnas [] = []
toColumnas f = transpose f

-- Funcion que separa el tablero en una lista de cuadrantes
toCuadrantes :: [[Int]] -> [[Int]]
toCuadrantes [] = []
toCuadrantes (a:b:as) = toCuadrantes' a b ++ toCuadrantes as 

-- Funcion auxiliar de toCuadrantes
toCuadrantes' :: [Int] -> [Int] -> [[Int]]
toCuadrantes' [] [] = []
toCuadrantes' a b = ((take 2 a)++(take 2 b)) : (toCuadrantes' (drop 2 a) (drop 2 b)) 


-- crea una tupla con indice de fila|columna|cuadrante y una 
-- lista con los valores de esa fila|columna|cuadrante
toTuplas :: [[Int]] -> [(Int , [Int])]
toTuplas lista = zip [0..] lista 


indices :: [[Int]] -> [(Int, [Int])]
indices [] = []
indices (l:ls) = [ (n ,(n `elemIndices` l)) | n <- [1..4] ] ++ indices ls 



-- Toma de parametros la fila vieja, el indice donde colocar el nuevo valor, y el valor en si. Reemplaza el valor 
-- unicamente si es distinto de cero (verificacion extra).
modificarValor :: [Int] -> Int -> Int -> [Int]
modificarValor lista indice valor 
						|head (drop indice lista) == 0 = (take (indice) lista) ++ valor : (drop (indice+1) lista)
						|otherwise = lista


-- Devuelve el indice de la lista que tiene valor 0.
getIndexUnico :: [Int] -> Int 
getIndexUnico lista = fromMaybe 1 (findIndex (==0) lista)

-- Devuelve el valor que todavia no está en la lista. Sirve tambien para los indices de donde no esta un valor.
valorFaltante :: [Int] -> [Int]
valorFaltante lista
				| null falta 			= []
				| otherwise 			= falta
				where falta = [ x | x <- [1..4], x `notElem` lista]

-- Tomamos el original y lo devolvemos modificado.
--evaluar :: [[Int]] -> [[Int]]
--evaluar cuad = valorFaltante (head cuad)

--evaluar' :: [[Int]] -> Int -> [Int] 
--evaluar' l x = findIndices (notElem x) l -- esto devuelve el indice de la lista donde NO esta el valor dado.
