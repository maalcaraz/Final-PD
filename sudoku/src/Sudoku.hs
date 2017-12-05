module Sudoku
    ( 
        tableroSudoku,
        resolverSudoku,
        printTablero
    ) where

import Data.Array

-- Valor : representa un valor del 0 al 9. 0 significa vacio
type Valor = Int
-- Cada cuadrado del tablero se identifica por su ubicacion la 
-- cual esta dada por (fila, columna)
type Ubicacion = (Int, Int)

-- Un tablero de sudokus esta definido como una matriz de 9x9 
-- y los valores definidos en cada uno de sus cuadrados
type Tablero = Array Ubicacion Valor

-- Retorna un tablero resuelto o 'Nothing' si no encuentra ninguna
resolverSudoku :: Tablero -> Maybe Tablero
resolverSudoku = solucionONothing . soluciones

solucionONothing :: [a] -> Maybe a
solucionONothing []     = Nothing
solucionONothing (x:xs) = Just x

-- Devuelve un array de Tableros donde cada uno es una solucion
soluciones :: Tablero -> [Tablero]
soluciones t = soluciones' (ubicacionesVacias t) t
  where
    -- Dada una lista de ubicaciones vacias en un tablero, toma una ubicacion vacia,
    -- determina que ubicaciones pueden ser puestas en esa solucion, y luego
    -- recursivamente busca todas las soluciones para ese conjunto de valores.
    soluciones' :: [Ubicacion] -> Tablero -> [Tablero]
    soluciones' []     t = [t]
    soluciones' (ub:ubs) t = concatMap (soluciones' ubs) tablerosPosibles
      where
        valoresPosibles = [v | v <- [1..9], isValidValor v ub t]
        tablerosPosibles = map (\v -> copiarTableroConValorNuevo v ub t) valoresPosibles

-- Retorna un array con todas las ubicaciones vacias
ubicacionesVacias :: Tablero -> [Ubicacion]
ubicacionesVacias t = [(row, col) | row <- [0..8], col <- [0..8], t ! (row, col) == 0]

-- Determina si el valor especificado puede ser puesto en una posicion
isValidValor :: Valor -> Ubicacion -> Tablero -> Bool
isValidValor v (row, col) t = rowOk && colOk && cuadradoOk
  where
    rowOk         = notElem v $ t `valsInRow` row
    colOk         = notElem v $ t `valsInColumn` col
    cuadradoOk    = notElem v $ t `valsInCuadrado` (row, col)

-- Asigna a un tablero un valor en una ubicacion especificada
copiarTableroConValorNuevo :: Valor -> Ubicacion -> Tablero -> Tablero
copiarTableroConValorNuevo valor (row, col) t = t // [((row, col), valor)]

-- Obtiene la lista de valores en la fila
valsInRow :: Tablero -> Int -> [Valor]
t `valsInRow` row = [t ! ub | ub <- range((row, 0), (row, 8))]

-- Obtiene la lista de valores en la columna
valsInColumn ::  Tablero -> Int -> [Valor]
t `valsInColumn` col = [t ! ub | ub <- range((0, col), (8, col))]

-- Obtiene la lista de valores en el sub-cuadrado que le corresponde
valsInCuadrado :: Tablero -> Ubicacion -> [Valor]
t `valsInCuadrado` (row, col) = [t ! ub | ub <- ubicaciones]
  where
    row' = (row `div` 3) * 3
    col' = (col `div` 3) * 3
    ubicaciones = range((row', col'), (row' + 2, col' + 2))

-- Convierte un array de filas de valores en un array de tuplas compuestas 
-- por ubicacion y valor
sudokuParser :: [[Valor]] -> [(Ubicacion, Valor)]
sudokuParser = concatMap rowParser . zip [0..8]
  where
    rowParser :: (Int, [Valor]) -> [((Int, Int), Valor)]
    rowParser (row, vals) = colParser row $ zip [0..8] vals

    colParser :: Int -> [(Int, Valor)] -> [((Int, Int), Valor)]
    colParser row cols = map (\(col, m) -> ((row, col), m)) cols

printTablero :: Maybe Tablero -> IO ()
printTablero Nothing  = putStrLn "No tiene solucion"
printTablero (Just b) = mapM_ putStrLn [show $ b `valsInRow` row | row <- [0..8]]

-- Devuelve un tablero de sudoku listo para procesarse
tableroSudoku :: Tablero
tableroSudoku = array ((0, 0), (8, 8)) $ sudokuParser sudokuEjemplo

-- Ejemplo sacado de: http://en.wikipedia.org/wiki/Sudoku
sudokuEjemplo :: [[Valor]]
sudokuEjemplo = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
                 [6, 0, 0,  1, 9, 5,  0, 0, 0],
                 [0, 9, 8,  0, 0, 0,  0, 6, 0],

                 [8, 0, 0,  0, 6, 0,  0, 0, 3],
                 [4, 0, 0,  8, 0, 3,  0, 0, 1],
                 [7, 0, 0,  0, 2, 0,  0, 0, 6],

                 [0, 6, 0,  0, 0, 0,  2, 8, 0],
                 [0, 0, 0,  4, 1, 9,  0, 0, 5],
                 [0, 0, 0,  0, 8, 0,  0, 7, 0]]