module Sudoku4x4V2
(
	leer,
	parsearEntrada,
	printAsMatriz,
	toFilas,
	start
	)where

import Data.Char
import Data.List
import System.IO
import Data.Maybe

leer :: String -> IO ()
leer nombre = do 
				s <- readFile nombre;
				let entrada = parsearEntrada s
				if isValid entrada 
					then do  
						writeFile "rs1.txt" ("Sudoku de entrada"++ '\n' : (printAsMatriz 4 (toFilas entrada)) ++ '\n':'\n':[])
						appendFile "rs1.txt"("Sudoku resuelto"++ '\n' :  (printAsMatriz 4 (start (toFilas entrada)))) 
					else do
						putStrLn "Ingreso mal los datos.."
						writeFile "rs1.txt" ("Sudoku de entrada"++ '\n' : (printAsMatriz 4 (toFilas entrada)) ++ '\n':'\n':[])




-- Toma un tablero que viene desde un archivo y lo convierte en una lista de enteros, suprimiento caracteres como '[', ']', ','
-- Esta funcion puede ser modificada para que directamente reconozca los numeros.
-- O mejor, para que elimine solamente los caracteres especiales.
parsearEntrada :: String-> [Int]
parsearEntrada [] = []
parsearEntrada (c:cs)
					| c == '-' = 0 : (parsearEntrada cs) 
					| isDigit c && (digitToInt(c) `elem` [0..4]) = digitToInt(c) : (parsearEntrada cs)
					|otherwise = parsearEntrada cs


-- Funcion que imprime el tablero en pantalla. Sus parámetros de entrada son: el tamaño del tablero y el tablero en forma de lista.
printAsMatriz :: Int -> [[Int]] -> String
printAsMatriz _ [] = " ________"
printAsMatriz n (l:ls) = " ________" ++ '\n' : ' ': (intersperse '|' (map (intToDigit) l)) ++ '\n' : (printAsMatriz n ls)


-- Funcion que enlista las filas del tablero
toFilas :: [a] -> [[a]]
toFilas [] = []
toFilas t = take 4 t : toFilas (drop 4 t) 

-- Funcion que enlista las columnas del tablero
toColumnas :: [[a]] -> [[a]]
toColumnas [] = []
toColumnas f = transpose f

-- Funcion que enlista los cuadrantes del tablero
toCuadrantes :: [[a]] -> [[a]]
toCuadrantes [] = []
toCuadrantes (a:b:as) = toCuadrantes' a b ++ toCuadrantes as 

-- Funcion auxiliar de toCuadrantes
toCuadrantes' :: [a] -> [a] -> [[a]]
toCuadrantes' [] [] = []
toCuadrantes' a b = ((take 2 a)++(take 2 b)) : (toCuadrantes' (drop 2 a) (drop 2 b)) 


-- Funcion que crea una lista de tuplas, donde cada tupla tiene el indice de fila|columna|cuadrante y una lista con los valores de esa fila|columna|cuadrante
toTuplas :: [[Int]] -> [(Int , [Int])]
toTuplas lista = zip [0..] lista 

-- Funcion que dada una tupla de fila|columna|cuadrante devuelve 
fromTuplas :: [(Int , [Int])] -> [[Int]]
fromTuplas [] = [] 
fromTuplas (t:ts) = (snd t) : fromTuplas ts

-- Toma como parametros las filas
toTablero :: [(Int, [Int])] -> [Int]
toTablero [] = []
toTablero ((_, l):as) = l ++ toTablero as

-- Toma como parametro todas las filas y columnas de un tablero.
-- Es el enlace entre cuadrantes, filas y columnas
completo :: [(Int, [Int])] -> [(Int, [Int])] -> [[(Int, [Int])]]
completo [] [] = []
completo [] b = []
completo filas cols = (completo' (take 2 filas) cols) ++ completo (drop 2 filas) cols


-- Toma como parámetro las dos primeras filas y todas las columnas
completo' :: [(Int, [Int])] -> [(Int, [Int])] -> [[(Int, [Int])]]
completo' [] [] = []
completo' a [] = []
completo' filas cols = ((take 2 filas)++(take 2 cols)):[] ++ completo' filas (drop 2 cols)   

-- ########################################################

-- Esta funcion devuelve True si el tablero esta lleno y False si no
-- Toma de parámetro un tablero.
isFull :: [Int] -> Bool
isFull l 
		| 0 `elem` l = False
		| otherwise = True

tableroValido :: [Int] -> Bool
tableroValido [] = False
tableroValido t 
				| length t == 16 = True
				| otherwise = False 


-- Funcion que verifica si un tablero ingresado es válido o no.
isValid :: [Int] -> Int -> Bool 
isValid entrada n
			|length entrada == n = True
			|otherwise = False

-- Devuelve el valor que todavia no está en la lista. Sirve tambien para los indices de donde no esta un valor.
valorFaltante :: [Int] -> [Int]
valorFaltante lista
				| null faltan 			= []
				| otherwise 			= faltan
				where faltan = [ x | x <- [1..4], x `notElem` lista]

-- Toma de parametros la fila vieja, el indice donde colocar el nuevo valor, y el valor en si. Reemplaza el valor 
-- unicamente si es distinto de cero (verificacion extra).
modificarValor :: [Int] -> Int -> Int -> [Int]
modificarValor lista indice valor 
						|head (drop indice lista) == 0 = (take (indice) lista) ++ valor : (drop (indice+1) lista)
						|otherwise = lista


-- Devuelve el indice de la lista que tiene valor 0. Es para el caso en que hay un solo valor faltante.
getIndexUnico :: [Int] -> Int 
getIndexUnico lista = fromMaybe 1 (findIndex (==0) lista)

-- la interseccion de los valores faltantes me tiene que dar el valor de una casilla.
-- Siempre y cuando la casilla este vacía.
valoresPosibles :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int]) -> [Int]
valoresPosibles cuadrante fila columna -- siempre y cuando el casillero sea == 0
									|(snd fila)!!(fst columna) == 0 = (valorFaltante (snd cuadrante) `intersect` valorFaltante (snd fila))  `intersect` valorFaltante (snd columna)
									|otherwise = []


--ESTRUCTURA PARA DEFINIR QUE FILAS Y COLUMNAS PERTENECEN A CADA CUADRANTE.

--DEL ESTILO:

--QO = F0,F1-C0,C1
--Q1 = F0,F1-C2,C3
--Q2 = F2,F3-C0,C1
--Q3 = F2,F3-C2,C3

-- Devuelve el tablero final
start :: [[Int]] ->  [[Int]]
start f 
	| isFull (toTablero (toTuplas f)) = f
	| otherwise = start $ (fromTuplas (resolver (toTuplas (toCuadrantes f)) (toTuplas f) (toTuplas (toColumnas f))) )



-- Tomo cuadrantes, filas y columnas
-- Devuelve las nuevas filas
resolver :: [(Int, [Int])] -> [(Int, [Int])] ->  [(Int, [Int])] -> [(Int, [Int])] 
resolver [] f _ = f
resolver (q:qs) f c = 
				let c1 = toTuplas (toColumnas (fromTuplas (f1)))
				    f1 = reemplazarFilas f (verificarCuadrante q ((completo f c)!!(fst q)))
				in resolver qs f1 c1 


reemplazarFilas :: [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])] 
reemplazarFilas f [] = f
reemplazarFilas filas (n:ns) = reemplazarFilas (reemplazarFila filas n) ns

reemplazarFila :: [(Int, [Int])] -> (Int, [Int]) -> [(Int, [Int])]
reemplazarFila filas f = (take (fst f) filas ++ f : drop ((fst f)+1) filas)

-- Funcion que toma 1 cuadrante con sus filas y columnas
-- Devuelve las filas que posiblemente se modificaron.
verificarCuadrante :: (Int, [Int]) -> [(Int, [Int])] -> [(Int, [Int])] 
verificarCuadrante (iQ, lQ) fc 
			| null vf 		=  take 2 fc -- Si el cuadrante esta lleno, devuelvo directamente las filas que vinieron
			| otherwise 	= (verificarCuadrante' vf (fc!!0) (drop 2 fc)) ++ (verificarCuadrante' vf (fc!!1) (drop 2 fc))
			where vf = valorFaltante lQ
	
-- Funcion que toma como parámetros una lista de los valores que faltan en el cuadrante, una fila y las columnas del cuadrante
verificarCuadrante' :: [Int] -> (Int, [Int]) -> [(Int, [Int])] -> [(Int, [Int])]
verificarCuadrante' [] f c = [f] 
verificarCuadrante' v f c = (modificarFila v (modificarFila v f (c!!1)) (c!!0)) : []


-- Funcion que toma como parámetros un valor posible, una fila, una columna y analiza si puede modificar la fila.
modificarFila :: [Int] -> (Int,[Int]) -> (Int, [Int]) -> (Int,[Int])
modificarFila v f c 
		| ((snd f)!!(fst c) /= 0) =	f 
		| ((length posible) == 1) && (elem (head posible) v) 					= ((fst f), modificarValor (snd f) (fst c) (head posible)) 
		| ((length posible) > 1) && ((length v) == 1) && ((head v) `elem` posible)	    = ((fst f), modificarValor (snd f) (fst c) (head v) ) 
		| otherwise = f
					where posible = [ x | x <- [1..4], x `notElem` (snd f), x `notElem` (snd c)]
