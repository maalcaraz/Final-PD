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