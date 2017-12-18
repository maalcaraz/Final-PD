module Sudoku
    ( 
        printAsMatriz
    ) where

import SudokuTypes
import Data.Char
import Data.Array
import Data.List


printAsMatriz :: Int -> [Int] -> String
printAsMatriz n [] | n == 4 = " -------"
                   | n == 9 = " -----------------"
                   | n == 16 = " ----------------------------------"
printAsMatriz n lista 
        | n == 4 = " _______" ++ '\n' : ' ' : auxPrint n lista
        | n == 9 = " _________________" ++ '\n' : ' ' : auxPrint n lista
        | n == 16 = " ________________________________" ++ '\n' : ' ' : auxPrint n lista


auxPrint :: Int -> [Int] -> String
auxPrint n lista = (intersperse '|' (map (intToDigit) (take n lista))) ++ '\n' : (printAsMatriz n(drop n lista))