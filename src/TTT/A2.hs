module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.String (IsString)


-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat["Player ", showSquare p, "'s turn: enter a row and column position (ex. A1)"] 


-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0.. _SIZE_ -1]

-- Q#03

isDigit :: Char -> Bool
isDigit c = 
    let charList = ['0' .. '9']
    in c `elem` charList 

readDigit :: Char -> Int
readDigit c 
    | isDigit c = read [c] 
    | otherwise = -1
    

-- Q#04

_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Neither 

_EMPTY_BOARD_ :: [[Square]]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied = undefined

_TIED_BOARD_ = undefined

-- Q#06

indexRowStrings = undefined

-- Q#07

formatLine = undefined

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined