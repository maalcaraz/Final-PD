-- Primer prueba, para cuadrados españoles.
 -- ['1','-', '-', '-','-','4', '-', '1','2','-', '1', '-','-','-', '-', '-']
import Data.Char
import System.IO
	
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

-- Funcion que separa el tablero en cuadros
toCuadros :: [[Int]] -> [[Int]]
toCuadros [] = []
toCuadros (a:b:as) = toCuadros' a b ++ toCuadros as 

-- Auxiliar de la funcion de arriba
toCuadros' :: [Int] -> [Int] -> [[Int]]
toCuadros' [] [] = []
toCuadros' a b = ((take 2 a)++(take 2 b)) : (toCuadros' (drop 2 a) (drop 2 b)) 
