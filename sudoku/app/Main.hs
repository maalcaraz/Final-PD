module Main where

import Sudoku9x9
import Sudoku4x4
import Sudoku16x16
import Data.Char
import Text.Read
import System.IO.Error
import ReadSudokuFile
import Data.Maybe
import Sys
import Sudoku4x4V2
import System.IO
import Control.Exception



main = toTry `catch` handler

toTry :: IO ()
toTry = do    
      putStrLn "===============BIENVENIDO A SUDOKU MASTER================"

		putStrLn "Nombre del archivo:"
      nombreFile <- getLine
		putStrLn "Ingrese el tamaño: "
      size <- getLine
      let sizeInt = readMaybe size :: Maybe Int
      if sizeInt == Nothing 
       then do putStrLn "FALLO"
      else do leerArchivo nombreFile (fromMaybe 0 sizeInt)

      putStrLn "Ingrese el tamaño del tablero (0 para salir): "
      size <- getLine

      let op = readMaybe size :: Maybe Int
      case op of
       Nothing -> do 
           putStrLn "No ingreso una opcion valida. Por favor vuelva a ingresar."
           main
       op -> do
           if op == (Just 0) 
               then do
                   putStrLn "CHAU"
                   return()
               else if op == (Just 4)
                   then do 
                       putStrLn "\n====================================================== \n"
                       putStrLn "Resolucion tablero 4x4 (Ejemplo 4): \n"
                       let sudokuResolution4x4ej4 = resolverSudoku4x4 $ tableroSudoku4x4 4
                       printSolucionTableros4x4 sudokuResolution4x4ej4
                       main
                   else if op == (Just 9)
                       then do
                           putStrLn "Ingresaste 9" 
                           main
                       else if op == (Just 16)
                           then do
                               putStrLn "Ingresaste 16"
                               main
                           else do 
                               putStrLn "Ingresaste un tamaño invalido, vuelve a intentarlo!"
                               main

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

--    putStrLn "\n====================================================== \n"
--    putStrLn "Resolucion tablero 9x9: \n"
--    let sudokuResolution9x9ej1 = resolverSudoku9x9 $ tableroSudoku9x9 1
--    printSolucionTableros9x9 sudokuResolution9x9ej1

--    putStrLn "\n====================================================== \n"
--    putStrLn "Resolucion tablero 9x9 vacio: \n"
--    let sudokuResolution9x9ej5 = resolverSudoku9x9 $ tableroSudoku9x9 5
--    printSolucionTableros9x9 sudokuResolution9x9ej5

--    putStrLn "\n====================================================== \n"
--    putStrLn "Resolucion tablero 9x9 con error: \n"
--    let sudokuResolution9x9ej4 = resolverSudoku9x9 $ tableroSudoku9x9 4
--    printSolucionTableros9x9 sudokuResolution9x9ej4

--    putStrLn "\n====================================================== \n"
--    putStrLn "Resolucion tablero 4x4 (Ejemplo 1): \n"
--    let sudokuResolution4x4ej1 = resolverSudoku4x4 $ tableroSudoku4x4 1
--    printSolucionTableros4x4 sudokuResolution4x4ej1

--    putStrLn "\n====================================================== \n"
--    putStrLn "Resolucion tablero 4x4 (Ejemplo 4): \n"
--    let sudokuResolution4x4ej4 = soluciones $ tableroSudoku4x4 4
--    let tablerosPrinteables = map toPrinteable sudokuResolution4x4ej4
--    let stringsTableros = map (printAsMatriz 4) tablerosPrinteables
--    let solucionesString = toFormatForSave stringsTableros 
--    writeFile "./src/out_files/asdasd.txt" stringGuardado

--    putStrLn "\n======================================================"
--    let sudokuResolution16x16 = resolverSudoku16x16 $ tableroSudoku16x16 1
--    printSolucionTablero16x16 sudokuResolution16x16
