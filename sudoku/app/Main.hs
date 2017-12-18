module Main where

import Sudoku
import qualified Sudoku.Sudoku4x4 as S4
import qualified Sudoku.Sudoku9x9 as S9
import qualified Sudoku.Sudoku16x16 as S16
import Data.Char
import Text.Read
import System.IO.Error
import ParserSudoku
import Data.Maybe
import System.IO
import Control.Exception


main = toTry `catch` handler

toTry :: IO ()
toTry = do
      putStrLn "\n==========================================================================================================\n"
      putStrLn "====================                       SUDOKU MASTER                         ========================="
      putStrLn "\n==========================================================================================================\n"

      putStrLn "Nombre del archivo entrada:"
      nombreFileEntrada <- getLine

      putStrLn "Ingrese el tamaño: "
      size <- getLine
      let sizeInt = readMaybe size :: Maybe Int

      putStrLn "Nombre del archivo salida:"
      nombreFileSalida <- getLine

      stringFileEntrada <- readFile nombreFileEntrada;

      if sizeInt == Nothing 
       then do 
        putStrLn "No ingreso un tamaño correcto. "
        volverAcomenzar
        main
      else do 
        let tam = (fromMaybe 0 sizeInt)
        let tablero = parsearEntrada tam stringFileEntrada
        if isValid tablero tam
            then do
                let tableroFormat = formatForParser tam tablero 
                writeFile nombreFileSalida ("Sudoku de entrada"++ '\n' : (printAsMatriz tam tableroFormat) ++ '\n':'\n':[])
                let sudokuResolution = case tam of {
                    (4) -> S4.soluciones $ S4.sudokuParser' tableroFormat;
                    (9) -> S9.soluciones $ S9.sudokuParser' tableroFormat;
                    (16) -> S16.soluciones $ S16.sudokuParser' tableroFormat;
                }

                let salida = if sudokuResolution == []
                                then "No tiene solucion"
                                else toStringTablero tam sudokuResolution

                appendFile nombreFileSalida ("Sudoku resuelto"++ '\n' : salida)
                
                putStrLn "\n==========================================================================================================\n"
                putStrLn "\nMuchas gracias por usar nuestro programa .. Su respuesta se encuentra en el archivo de salida elegido\n"
                putStrLn "\n==========================================================================================================\n"
                volverAcomenzar
                main
            else if tam `notElem` [4,9,16]
                then do
                    putStrLn "El tamaño ingresado no esta dentro de los validos , debe ser (4,9,16) .. Vuelve a intentarlo !\n\n"
                    volverAcomenzar
                    main
                else do
                    putStrLn "El archivo no posee un formato de sudoku valido .. Vuelve a intentarlo! \n\n"
                    volverAcomenzar
                    main--}

volverAcomenzar :: IO ()
volverAcomenzar = do putStrLn "Presione una tecla para volver a comenzar ..."
                     syspause <- getLine
                     putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = do 
        putStrLn "El archivo no existe!"
        volverAcomenzar
        main
    | isAlreadyInUseError e = do
        putStrLn "No puedes guardar en un archivo de lectura!"
        volverAcomenzar
        main
    | otherwise = ioError e



{--
let nombreFileEntrada = "./src/entry_files/FileSudokuTest4x4_01.txt"
let nombreFileEntrada = "./src/entry_files/Test4x4_01.txt"
let nombreFileEntrada = "./src/entry_files/test16x16_01.txt"
let nombreFileSalida = "./src/out_files/FileSudokuTest4x4_01.txt"
let sizeInt = Just 4
--}
{--
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
                       let sudokuResolution4x4ej4 = S4.resolverSudoku4x4 $ S4.tableroSudoku4x4 4
                       S4.printSolucionTableros4x4 sudokuResolution4x4ej4
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
--}
{--
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
    let sudokuResolution4x4ej4 = soluciones $ tableroSudoku4x4 4
    let tablerosPrinteables = map toPrinteable sudokuResolution4x4ej4
    let stringsTableros = map (printAsMatriz 4) tablerosPrinteables
    let solucionesString = toFormatForSave stringsTableros 
    writeFile "./src/out_files/asdasd.txt" stringGuardado
    putStrLn "\n======================================================"
      let sudokuResolution16x16 = S16.resolverSudoku $ S16.tableroSudoku 2
      S16.printSolucionTableros sudokuResolution16x16
--}