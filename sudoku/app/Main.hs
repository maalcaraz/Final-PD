module Main where

import Sudoku

main :: IO ()
main = do
    printSolucionTablero $ resolverSudoku tableroSudoku