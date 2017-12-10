-- Primer prueba, para cuadrados españoles.
 -- ['1','-', '-', '-','-','4', '-', '1','2','-', '1', '-','-','-', '-', '-']
import Data.Char
import System.IO
import Data.Maybe
	
main :: IO ()
main = do 
  let pv = primeraPasada ['1','-', '-', '-','-','4', '-', '1','2','-', '1', '-','-','-', '-', '-']
  putStrLn (printAsMatriz pv)
	
-- Toma el tablero como viene, y coloca ceros en los espacios vacíos
primeraPasada :: [Char] -> [Int]
primeraPasada [] = []
primeraPasada (a:as)
		| a == '-' = 0 : (primeraPasada as) 
		|otherwise = digitToInt(a) : (primeraPasada as)

-- Funcion que imprime el tablero en pantalla
printAsMatriz :: [Int] -> String
printAsMatriz [] = " ________"
printAsMatriz (a:b:c:d:as) = " ________" ++ '\n' : '|' : (intToDigit a) : '|': (intToDigit b) : '|': (intToDigit c) : '|': (intToDigit d) : '|' :'\n' : [] ++ printAsMatriz as

-- Funcion que separa el tablero en filas
toFilas :: [Int] -> [[Int]]
toFilas [] = []
toFilas t = take 4 t : toFilas (drop 4 t) 

-- Funcion que arma las columnas del tablero a partir de las filas
toColumnas :: [[Int]] -> [[Int]]
toColumnas [] = []
toColumnas f = transpose f

-- Funcion que separa el tablero en cuadrantes
toCuadrantes :: [[Int]] -> [[Int]]
toCuadrantes [] = []
toCuadrantes (a:b:as) = toCuadrantes' a b ++ toCuadrantes as 
-- Auxiliar de la funcion de arriba
toCuadrantes' :: [Int] -> [Int] -> [[Int]]
toCuadrantes' [] [] = []
toCuadrantes' a b = ((take 2 a)++(take 2 b)) : (toCuadrantes' (drop 2 a) (drop 2 b)) 


-- crea una tupla con indice de fila|columna|cuadrante y una 
-- lista con los valores de esa fila|columna|cuadrante
toTuplas :: [[Int]] -> [(Int , [Int])]
toTuplas lista = zip [1..] lista 

indices :: [[Int]] -> [(Int, [Int])]
indices [] = []
indices (l:ls) = [ (n ,(n `elemIndices` l)) | n <- [1..4] ] ++ indices ls 


-- Toma de parametros la fila vieja, el indice donde colocar
-- el nuevo valor, y el valor en si. Reemplaza el valor unicamente si
-- es distinto de cero.
modificarValor :: [Int] -> Int -> Int -> [Int]
modificarValor lista indice valor 
			|head (drop indice lista) == 0 = (take (indice) lista) ++ valor : (drop (indice+1) lista)
			|otherwise = lista

-- Devuelve el indice de la lista que tiene valor 0.
getIndexUnico :: [Int] -> Int 
getIndexUnico lista = fromMaybe 1 (findIndex (==0) lista)

-- (snd (indicesFilas !!3!!0)) `intersect` (snd (indicesCols !!0!!0))


-- ***la combinacion de estas funciones me va a decir la posicion
-- donde NO esta un numero dado. En el ejemplo es 1.

-- findIndices (elem 1) c -> [0,2,3]
-- findIndices (elem 1) f -> [0,1,2]
-- findIndices (elem 1) cuadrantes  -> [0,1,3]


--Resultado: Columna 1, fila 3, cuadrante 2.

-- FALTA

-- Agregar el metodo de insercion de un valor una vez que descubri
-- la posicion en donde va.

