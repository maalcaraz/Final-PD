module Sudoku.Sudoku16x16
    ( 
        resolverSudoku,
        tableroSudoku,
        printSolucionTableros,
        sudokuParser,
        sudokuParser',
        soluciones
    ) where

import Data.Array
import Tests16x16
import SudokuTypes

-- Retorna x cant de tableros resueltos o 'Nothing' si no encuentra ninguno
resolverSudoku :: Tablero -> [Maybe Tablero]
resolverSudoku = tablerosToMaybe . soluciones

tablerosToMaybe :: [Tablero] -> [Maybe Tablero]
tablerosToMaybe [] = [Nothing]
tablerosToMaybe ts = map tableroToMaybe ts

tableroToMaybe :: Tablero -> Maybe Tablero
tableroToMaybe t = Just t

-- Devuelve un array de Tableros donde cada uno es una solucion
soluciones :: Tablero -> [Tablero]
soluciones t = take 10 ( soluciones' (ubicacionesVacias t) t )
  where
    -- Dada una lista de ubicaciones vacias en un tablero, toma una ubicacion vacia,
    -- determina que ubicaciones pueden ser puestas en esa solucion, y luego
    -- recursivamente busca todas las soluciones para ese conjunto de valores.
    soluciones' :: [Ubicacion] -> Tablero -> [Tablero]
    soluciones' []     t = [t]
    soluciones' (ub:ubs) t = concatMap (soluciones' ubs) tablerosPosibles
      where
        tablerosPosibles = map (\vp -> copiarTableroConValorNuevo vp ub t) valoresPosibles
        valoresPosibles = [v | v <- [1..16], isValidValor v ub t]
        

-- Retorna un array con todas las ubicaciones vacias
ubicacionesVacias :: Tablero -> [Ubicacion]
ubicacionesVacias t = [(row, col) | row <- [0..15], col <- [0..15], t ! (row, col) == 0]
        -- con las listas intensionales vemos como es muy facil encontrar
        -- las psoiciones con 0 o vacias. t!(row,col) => devuelve el val en esa posicion

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
t `valsInRow` row = [t ! v | v <- range((row, 0), (row, 15))]

-- Obtiene la lista de valores en la columna
valsInColumn ::  Tablero -> Int -> [Valor]
t `valsInColumn` col = [t ! v | v <- range((0, col), (15, col))]

-- Obtiene la lista de valores en el sub-cuadrado que le corresponde
valsInCuadrado :: Tablero -> Ubicacion -> [Valor]
t `valsInCuadrado` (row, col) = [t ! v | v <- ubicaciones]
  where
    row' = (row `div` 4) * 4
    col' = (col `div` 4) * 4
    ubicaciones = range((row', col'), (row' + 3, col' + 3))


-- recibe un Maybe Tablero porque la idea es que sea llamado desde 
-- la funcion resolverSudoku la cual devuelve Nothing si no puede
-- resolverlo o un Just en caso de que si
printSolucionTablero :: Maybe Tablero -> IO ()
printSolucionTablero Nothing  = putStrLn "No tiene solucion"
printSolucionTablero (Just t) = mapM_ putStrLn ([show $ t `valsInRow` row | row <- [0..15]] ++ ["\n"])
        -- por cada una de las rows se devuelve un string gracias al uso de la
        -- funcion show y ejecutamos el putStrLn para que se muestre una debajo
        -- de la otra. Logramos mostrar todas las lineas gracias a mapM_
        -- mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()

printSolucionTableros :: [Maybe Tablero] -> IO ()
printSolucionTableros ts = mapM_ printSolucionTablero ts 

-- Devuelve un tablero de sudoku listo para procesarse
tableroSudoku :: Int -> Tablero
tableroSudoku 1 = array ((0, 0), (15, 15)) $ sudokuParser sudokuEjemplo1
tableroSudoku 2 = array ((0, 0), (15, 15)) $ sudokuParser sudokuEjemplo2
tableroSudoku x = array ((0, 0), (15, 15)) $ sudokuParser emptySudoku
                -- se reserva un espacio en memoria con toda la combinacion de
                -- de indices desde 0,0 hasta el 15,15 y luego se rellena con
                -- lo que devuelve el metodo sudokuParser el cual pasa un 
                -- array de array de Int a un formato de Tablero definido mas arriba

sudokuParser' :: [[Valor]] -> Tablero
sudokuParser' sud = array ((0, 0), (15, 15)) $ concatMap rowParser $ zip [0..15] sud
    where
    rowParser :: (Int, [Valor]) -> [((Int, Int), Valor)]
    rowParser (row, vals) = colParser row $ zip [0..15] vals

    colParser :: Int -> [(Int, Valor)] -> [((Int, Int), Valor)]
    colParser row colsAndVals = map (\(col, v) -> ((row, col), v)) colsAndVals


-- Convierte un array de filas de valores en un array de tuplas compuestas 
-- por ubicacion y valor (Es del tipo Tablero)
sudokuParser :: [[Valor]] -> [(Ubicacion, Valor)]
sudokuParser sud = concatMap rowParser $ zip [0..15] sud
        -- zip devuelve [(0,[1,2,3,4,5,6,7,15,16]), .. ] donde cada valor
        -- es (Int,[Valor]) parametro que recibe rowParser que con concatMap
        -- lo que hacemos es mandarle cada uno de los elementos del array
        -- y luego acumular el resultado en otro array
  where
    rowParser :: (Int, [Valor]) -> [((Int, Int), Valor)]
    rowParser (row, vals) = colParser row $ zip [0..15] vals

    colParser :: Int -> [(Int, Valor)] -> [((Int, Int), Valor)]
    colParser row colsAndVals = map (\(col, v) -> ((row, col), v)) colsAndVals
    -- por cada una de los cols defino (col,v) donde (row,col) representa el
    -- indice y v representa es el valor