module Main where

import Sudoku
import Sudoku4x4
import Sudoku16x16

main :: IO ()
main = do
    let sudokuResolution9x9 = resolverSudoku $ tableroSudoku 1
    printSolucionTablero sudokuResolution9x9
    putStrLn "======================================================"
    let sudokuResolution4x4 = resolverSudoku4x4 $ tableroSudoku4x4 1
    printSolucionTablero4x4 sudokuResolution4x4
    putStrLn "======================================================"
    --let sudokuResolution16x16 = resolverSudoku16x16 $ tableroSudoku16x16 1
    --printSolucionTablero16x16 sudokuResolution16x16