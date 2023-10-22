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
isTied :: Board -> Bool
isTied b = 
    let x = concat b         
    in 
        if Neither `notElem` x then True
        else False

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
     [X,O,X]
    ,[X,O,O]
    ,[O,X,O]
    ]

_RANDOM_BOARD_ :: Board
_RANDOM_BOARD_ = [
     [X,O,Neither]
    ,[X,Neither,O]
    ,[Neither,X,O]
    ]

-- Q#06
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings x = zipWith (\a b -> (a,b)) ['A'..] x    


-- Q#07
formatLine :: [String] -> String
formatLine xs =  _SEP_ ++ intercalate _SEP_ xs ++ _SEP_       


-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x,y) 
    | x < _SIZE_ && x >= 0 && y < _SIZE_ && y >= 0    = True
    | otherwise                                         = False

-- Q#09
stringToMove :: String -> Move
stringToMove (x:xs:[]) = 
    let first = convertRowIndex x 
        --c = head xs
        second = readDigit xs
        validMove = isMoveInBounds (first,second)
    in  if validMove then (first,second)
        else _INVALID_MOVE_
stringToMove _ = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row 
replaceSquareInRow player index row
    | index >= length row || index < 0 = row 
    | otherwise = 
        let (first,_:second) = splitAt index row 
        in first ++ [player] ++ second 

rsX :: Int -> Row -> Row 
rsX index row = replaceSquareInRow X index row

rsO :: Int -> Row -> Row 
rsO index row = replaceSquareInRow O index row 
