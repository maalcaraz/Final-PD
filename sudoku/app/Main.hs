module Main where

import Sudoku9x9
import Sudoku4x4
import Sudoku16x16

main :: IO ()
main = do
    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9: \n"
    let sudokuResolution9x9ej1 = resolverSudoku9x9 $ tableroSudoku9x9 1
    printSolucionTableros9x9 sudokuResolution9x9ej1

    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9 vacio: \n"
    let sudokuResolution9x9ej5 = resolverSudoku9x9 $ tableroSudoku9x9 5
    printSolucionTableros9x9 sudokuResolution9x9ej5

    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 9x9 con error: \n"
    let sudokuResolution9x9ej4 = resolverSudoku9x9 $ tableroSudoku9x9 4
    printSolucionTableros9x9 sudokuResolution9x9ej4
    
    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 4x4 (Ejemplo 1): \n"
    let sudokuResolution4x4ej1 = resolverSudoku4x4 $ tableroSudoku4x4 1
    printSolucionTableros4x4 sudokuResolution4x4ej1

    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 4x4 (Ejemplo 4): \n"
    let sudokuResolution4x4ej4 = resolverSudoku4x4 $ tableroSudoku4x4 4
    let cadenaSudoku = printSolucionTableros4x4 sudokuResolution4x4ej4


    putStrLn "\n====================================================== \n"
    putStrLn "Resolucion tablero 4x4 (Ejemplo 4): \n"
    let sudokuResolution4x4ej4 = soluciones $ tableroSudoku4x4 4
    let tablerosPrinteables = map toPrinteable sudokuResolution4x4ej4
    let stringsTableros = map (printAsMatriz 4) tablerosPrinteables
    let solucionesString = toFormatForSave stringsTableros 
    writeFile "./src/out_files/asdasd.txt" stringGuardado

    --putStrLn "\n======================================================"
    --let sudokuResolution16x16 = resolverSudoku16x16 $ tableroSudoku16x16 1
    --printSolucionTablero16x16 sudokuResolution16x16