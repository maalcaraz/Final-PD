module ReadSudokuFile 
    (
        leerArchivo
    )
    where

import Data.Char
import System.IO
import Control.Applicative
import Data.Array
import Data.List
import SudokuTypes
import Sudoku

leerArchivo :: String -> Int -> IO()
leerArchivo nombre size = do
    s <- readFile nombre;
    let strTablero = printAsMatriz size (parsearEntrada size s)
    putStrLn strTablero

parsearEntrada :: Int -> String -> [Int]
parsearEntrada size [] = []
parsearEntrada size (c:cs)
                    | c == '-' = 0 : (parsearEntrada size cs) 
                    | isDigit c && (digitToInt(c) `elem` [0..size]) = digitToInt(c) : (parsearEntrada size cs)
                    | otherwise = parsearEntrada size cs


--fromArrIntToTablero :: Int -> [Int] -> Tablero
--fromArrIntToTablero size [] = array ((0, 0), (8, 8)) $ sudokuParser [[0]] 
--fromArrIntToTablero size ls | (length ls) == (size * size) && size == 9 = array ((0, 0), (8, 8)) $ sudokuParser ( formatForParser size ls )
                            -- | (length ls) == (size * size) && size == 4 = array ((0, 0), (3, 3)) $ sudokuParser ( formatForParser size ls )
                            -- | otherwise = array ((0, 0), (8, 8)) $ sudokuParser [[0]] 


--formatForParser :: Int -> [Int] -> [[Int]]
--formatForParser size [] = []
--formatForParser size ls = (take size ls) :  (formatForParser size (drop size ls))



