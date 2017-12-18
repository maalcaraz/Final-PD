module Sudoku
    ( 
        printAsMatriz,
        toFilas,
        toPrinteable,
        toStringTablero
    ) where

import SudokuTypes
import Data.Char
import Data.Array
import Data.List

printAsMatriz :: Int -> [[Int]] -> String
printAsMatriz n [] | n == 4 = " -------\n\n"
                   | n == 9 = " -----------------\n\n"
                   | n == 16 = " ----------------------------------\n\n"
printAsMatriz n lista 
        | n == 4 = " _______" ++ '\n' : ' ' : auxPrint n lista
        | n == 9 = " _________________" ++ '\n' : ' ' : auxPrint n lista
        | n == 16 = " ________________________________" ++ '\n' : ' ' : auxPrint n lista
        where
            auxPrint n (l:ls) = (intersperse '|' (map intToDigit l)) ++ '\n' : (printAsMatriz n ls)
                    
-- Lleva un tablero a un formato que pueda ser tomado por el metodo prinAsMatriz
toPrinteable :: Int -> Tablero -> [[Valor]]
toPrinteable tam t =  toFilas tam [t ! (row, col) | row <- [0..tam-1], col <- [0..tam-1]] 

-- Funcion que enlista las filas del tablero
toFilas :: Int -> [a] -> [[a]]
toFilas tam [] = []
toFilas tam t = take tam t : toFilas tam (drop tam t) 

toStringTablero :: Int -> [Tablero] -> String
toStringTablero tam tableros =  concat [ printAsMatriz tam (toPrinteable tam t) | t <- tableros ]
