module Main where

import Sudoku9x9
import Sudoku4x4
import Sudoku16x16

main :: IO ()
main = do
    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9: \n"
    let sudokuResolution9x9Ok = resolverSudoku $ tableroSudoku 1
    printSolucionTablero sudokuResolution9x9Ok

    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9 vacio: \n"
    let sudokuResolution9x9Vacio = resolverSudoku $ tableroSudoku 5
    printSolucionTablero sudokuResolution9x9Vacio

    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9 con error: \n"
    let sudokuResolution9x9Fail = resolverSudoku $ tableroSudoku 4
    printSolucionTablero sudokuResolution9x9Fail
    
    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 4x4: \n"
    let sudokuResolution4x4 = resolverSudoku4x4 $ tableroSudoku4x4 1
    printSolucionTablero4x4 sudokuResolution4x4
    
    putStrLn "\n======================================================"
    --let sudokuResolution16x16 = resolverSudoku16x16 $ tableroSudoku16x16 1
    --printSolucionTablero16x16 sudokuResolution16x16