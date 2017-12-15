module SudokuTypes where

import Data.Array

-- Valor : representa un valor del 0 al 9. 0 significa vacio
type Valor = Int

-- Cada cuadrado del tablero se identifica por su ubicacion la 
-- cual esta dada por (fila, columna)
type Ubicacion = (Int, Int)

-- Un tablero de sudokus esta definido como una matriz de 9x9 
-- y los valores definidos en cada uno de sus cuadrado. Se define
-- como un [((0,0),1) , .. ]
type Tablero = Array Ubicacion Valor
