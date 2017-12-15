module ReadSudokuFile where

import Data.Char
import System.IO
import Control.Applicative
import Data.List

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


--contenido :: String -> Integer-
--contenido [] = 0
--contenido cont = cont

printAsMatriz :: Int -> [Int] -> String
printAsMatriz _ [] = " __________________"
printAsMatriz n (lista) = " __________________" ++ '\n' : ' ': (intersperse '|' (map (intToDigit) (take n lista))) ++ '\n' : (printAsMatriz n (drop n lista))

parsearEntrada :: String-> [Int]
parsearEntrada [] = []
parsearEntrada cadena
                    | (head cadena) == '-' = 0 : (parsearEntrada (tail cadena)) 
                    | ((head cadena)=='[')||((head cadena)==']') || ((head cadena)==',') = parsearEntrada (tail cadena)
                    |otherwise = digitToInt((head cadena)) : (parsearEntrada (tail cadena))


