module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Text.Read (Lexeme(String))
import Control.Monad (forM)

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ =  let str = showInts _RANGE_
            in formatLine str   

-- Q#02
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = show x : showSquares xs  

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = [] 
formatRows (x:xs) = formatLine (go x) : formatRows xs 
    where   
        go :: Row -> [String]
        go [] = [] 
        go y = showSquares y 

-- Q#04

isColEmpty :: Row -> Int -> Bool 
isColEmpty [] _ = False
isColEmpty (x:xs) 0  = x == Neither  
isColEmpty (x:xs) n = isColEmpty xs (n-1) 
-- r = [X,O,Neither]
-- isColEmpty (X:[O,Neither]) 2 = isColEmpty [O,Neither] (2-1)
-- isColEmpty (O:Neither) 1 = isColEmpty [Neither] (1-1)
-- isColEmpty (Neither: []) 0 = Neither == Neither
-- result is true

-- Q#05
dropFirstCol :: Board -> Board 
dropFirstCol []=[]
dropFirstCol (x:xs) = tail x : dropFirstCol xs  

dropLastCol :: Board -> Board 
dropLastCol [] = []
dropLastCol (x:xs) = init x : dropLastCol xs

-- Q#06

getDiag1 = undefined

getDiag2 = undefined

getAllLines = undefined

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined