module Main where

import Sudoku

main :: IO ()
main = do
    let solucion = resolverSudoku tableroSudoku
    printTablero solucion