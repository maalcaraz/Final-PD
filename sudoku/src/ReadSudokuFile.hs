module ReadSudokuFile where

import Data.Char
import System.IO
import Control.Applicative
import Data.Array
import Data.List
import SudokuTypes
import Sudoku4x4

leerArchivo :: String -> Int -> IO ()
leerArchivo nombre size = do 
                     s <- readFile nombre; -- donde nombre es el filename
                     putStrLn "\n============================================================================================================================================================== \n"
                     putStrLn "\t\t\t\tString de entrada \n"
                     putStrLn s
                     putStrLn "\n============================================================================================================================================================== \n"
                     let arrVals = parsearEntrada s
                     putStrLn "\t\t\t\tArreglo de Int \n"
                     print arrVals
                     putStrLn "\n============================================================================================================================================================== \n"
                     putStrLn "\t\t\t\tSudoku parsed \n"
                     putStrLn (printAsMatriz size arrVals)
                     putStrLn "\n============================================================================================================================================================== \n"
                     putStrLn "\t\t\t\tSudoku parsed \n"
                     putStrLn (printAsMatriz size arrVals)
                     putStrLn "\n============================================================================================================================================================== \n"



printAsMatriz :: Int -> [Int] -> String
printAsMatriz _ [] = " __________________"
printAsMatriz n (lista) = " __________________" ++ '\n' : ' ': (intersperse '|' (map (intToDigit) (take n lista))) ++ '\n' : (printAsMatriz n (drop n lista))

parsearEntrada :: String-> [Int]
parsearEntrada [] = []
parsearEntrada c:tailCadena 
                    | c == '[' || c == ']' || c == ',' = parsearEntrada tailCadena
                    | otherwise = digitToInt c : parsearEntrada tailCadena



fromArrIntToTablero :: Int -> [Int] -> Tablero
fromArrIntToTablero size [] = array ((0, 0), (8, 8)) $ sudokuParser [[0]] 
fromArrIntToTablero size ls | (length ls) == (size * size) && size == 9 = array ((0, 0), (8, 8)) $ sudokuParser ( formatForParser size ls )
                            | (length ls) == (size * size) && size == 4 = array ((0, 0), (3, 3)) $ sudokuParser ( formatForParser size ls )
                            | otherwise = array ((0, 0), (8, 8)) $ sudokuParser [[0]] 


formatForParser :: Int -> [Int] -> [[Int]]
formatForParser size [] = []
formatForParser size ls = (take size ls) :  (formatForParser size (drop size ls))



