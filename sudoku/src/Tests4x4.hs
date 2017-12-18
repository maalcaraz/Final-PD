module Tests4x4
    ( 
        sudokuEjemplo1,
        sudokuEjemplo2,
        sudokuEjemplo3,
        sudokuEjemplo4,
        emptySudoku
    ) where

import SudokuTypes

-- Ejemplo sacado de: http://en.wikipedia.org/wiki/Sudoku
sudokuEjemplo1 :: [[Valor]]
sudokuEjemplo1 = [
                 [1,0, 0,0],
                 [0,4, 0,1],
                 [2,0, 1,0],
                 [0,0, 0,0]]

sudokuEjemplo2 :: [[Valor]]
sudokuEjemplo2 = [[0,0, 0,0],
                 [0,0, 0,0],
                 [0,0, 0,0],
                 [0,0, 0,0]]


sudokuEjemplo3 :: [[Valor]]
sudokuEjemplo3 = [[0,0, 0,0],
                 [0,0, 0,0],
                 [0,0, 0,0],
                 [0,0, 0,0]]

sudokuEjemplo4 :: [[Valor]]
sudokuEjemplo4 = [
                  [1,0, 0,0],
                  [3,0, 0,0],
                 
                  [0,3, 0,4],
                  [0,1, 0,0]
                 ]

emptySudoku :: [[Valor]]
emptySudoku = [[0,0, 0,0],
               [0,0, 0,0],
               [0,0, 0,0],
               [0,0, 0,0]]