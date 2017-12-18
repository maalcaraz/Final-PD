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
      putStrLn "==================== SUDOKU MASTER ========================="

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
        putStrLn "No ingreso un tamaño correcto. Adios"
        return()
      else do 
        let tam = (fromMaybe 0 sizeInt)
        let tablero = parsearEntrada tam stringFileEntrada
        if isValid tablero tam
            then do
                let tableroFormat = formatForParser tam tablero 
                writeFile nombreFileSalida ("Sudoku de entrada"++ '\n' : (printAsMatriz tam tableroFormat) ++ '\n':'\n':[])
                let sudokuResolution = case tam of {
                    (4) -> S4.soluciones $ S4.sudokuParser4x4' tableroFormat;
                    (9) -> S9.soluciones $ S9.sudokuParser9x9' tableroFormat;
                }

                let salida = if sudokuResolution == []
                                then "No tiene solucion"
                                else toStringTablero tam sudokuResolution

                appendFile nombreFileSalida ("Sudoku resuelto"++ '\n' : salida)
                
                putStrLn "Muchas gracias por usar nuestro programa .. \n\n"
                main
            else if tam `notElem` [4,9,16]
                then do
                    putStrLn "El tamaño ingresado no esta dentro de los validos , debe ser (4,9,16) .. Vuelve a intentarlo !\n\n"
                    main
                else do
                    putStrLn "El archivo no posee un formato de sudoku valido .. Vuelve a intentarlo! \n\n"
                    main


handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = do 
        putStrLn "El archivo no existe!"
        main
    | isAlreadyInUseError e = do
        putStrLn "No puedes guardar en un archivo de lectura!"
        main
    | otherwise = ioError e




--let nombreFileEntrada = "./src/entry_files/FileSudokuTest4x4_01.txt"
--let nombreFileEntrada = "./src/entry_files/Test4x4_01.txt"
--let nombreFileSalida = "./src/out_files/FileSudokuTest4x4_01.txt"
--let sizeInt = Just 4


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
