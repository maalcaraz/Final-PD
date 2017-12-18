module ParserSudoku
    (
        parsearEntrada,
        fromArrIntToTablero,
        formatForParser,
        isValid
    )
    where

import Data.Char
import System.IO
import Control.Applicative
import Data.Array
import Data.List
import SudokuTypes
import Sudoku
import qualified Sudoku.Sudoku4x4 as S4
import qualified Sudoku.Sudoku9x9 as S9
import qualified Sudoku.Sudoku16x16 as S16

-- Lee un String y lo transforma en un array de Int para que luego pueda ser transformado a un tablero y resuelto respectivamente
parsearEntrada :: Int -> String -> [Int]
parsearEntrada size [] = []
parsearEntrada size (c:cs)
                    | c == '-' = 0 : (parsearEntrada size cs) 
                    | isDigit c && (digitToInt(c) `elem` [0..size]) = digitToInt(c) : (parsearEntrada size cs)
                    | otherwise = parsearEntrada size cs

-- Funcion que verifica si un tablero ingresado es válido o no.
isValid :: [Int] -> Int -> Bool 
isValid entrada n
            | length entrada == n*n = True
            | otherwise = False


-- Dado un array de int y el tamaño te devuelve un tablero
fromArrIntToTablero :: Int -> [Int] -> Tablero
fromArrIntToTablero size [] | size == 4 = array ((0, 0), (size-1, size-1)) $ S4.sudokuParser [[0]] 
                            | size == 9 = array ((0, 0), (size-1, size-1)) $ S9.sudokuParser [[0]]
                            | size == 16 = array ((0, 0), (size-1, size-1)) $ S16.sudokuParser [[0]]

fromArrIntToTablero size ls | (length ls) == (size * size) && size == 4 = array ((0, 0), (3, 3)) $ S4.sudokuParser ( formatForParser size ls )
                            | (length ls) == (size * size) && size == 9 = array ((0, 0), (8, 8)) $ S9.sudokuParser ( formatForParser size ls )
                            | (length ls) == (size * size) && size == 16 = array ((0, 0), (15, 15)) $ S16.sudokuParser ( formatForParser size ls )
                            | size == 4 = array ((0, 0), (size-1, size-1)) $ S4.sudokuParser [[0]] 
                            | size == 9 = array ((0, 0), (size-1, size-1)) $ S9.sudokuParser [[0]]
                            | size == 16 = array ((0, 0), (size-1, size-1)) $ S16.sudokuParser [[0]]


formatForParser :: Int -> [Int] -> [[Int]]
formatForParser size [] = []
formatForParser size ls = (take size ls) :  (formatForParser size (drop size ls))
