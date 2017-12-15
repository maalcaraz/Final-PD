module Main where

import Sudoku

main :: IO ()
main = do
    let sudokuResuelution = resolverSudoku $ tableroSudoku 1
    printSolucionTablero sudokuResuelution