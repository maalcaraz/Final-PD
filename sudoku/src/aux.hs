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

toFilas :: [Int] -> [[Int]]
toFilas [] = []
toFilas t = take 4 t : toFilas (drop 4 t) 
			
				
				
				
				
				
				
				
