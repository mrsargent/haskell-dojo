module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Text.Read (Lexeme(String))
import Control.Monad (forM)
import System.Random.Stateful (globalStdGen)


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
-- Board = [Row]
-- Row =  [Square]
-- Line = [Square]
-- Move = (Int,Int)
-- Player = Square
getDiag1 :: Board -> Line 
getDiag1 [] = []
getDiag1 (x:xs) = head x : go 
    where
       go = getDiag1 (dropFirstCol xs)
--getDiag1 xs = go xs 0
--   where
--        go :: Board -> Int -> Line 
--        go [] _ = []
--        go (y:ys) 0 = head y : go ys 1
--        go (y:ys) n = y !! n : go ys (n+1)
       
getDiag2 :: Board -> Line          
getDiag2 [] = []
getDiag2 (x:xs) = last x : go 
    where 
        go = getDiag2 (dropLastCol xs)


getAllLines :: Board -> [Line]
getAllLines b = b ++ transpose b ++ [getDiag1 b] ++ [getDiag2 b]

-- Q#07
putSquare :: Player -> Board -> Move -> Board 
putSquare _ [] _ = []
putSquare player (x:xs) (0,y) = replaceSquareInRow player y x : xs 
putSquare player (x:xs) (n,y) = x : putSquare player xs (n-1,y) 
-- putSquare X _EMPTY_BOARD_ (1,1) 
-- putSquare X ([E,E,E]:[[E,E,E],[E,E,E]]) (1,1) = [E,E,E] : putSquare X [[E,E,E],[E,E,E]] (1-1,1)
-- putSqaure X ([E,E,E]:[E,E,E]) (0,1) = replaceSquareInRow X 1 [E,E,E] : [E,E,E]
-- result [[Neither,Neither,Neither],[Neither,X,Neither],[Neither,Neither,Neither]]

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices x = map combineTupleToString (indexRowStrings x)
                        where 
                            combineTupleToString :: (Char,String) -> String 
                            combineTupleToString (c,s) = c : s 

-- Q#09

isWinningLine :: Player -> Line -> Bool 
isWinningLine _ [] = False 
isWinningLine player xs = go False player xs 
    where
        go :: Bool -> Player -> Line -> Bool 
   --    go True _ [] = True 
        go _ p (y:ys) = y == p && go True p ys 
        go bool _ [] = bool 


-- Q#10
-- Board = [Row]
-- Row =  [Square]
-- Line = [Square]
-- Move = (Int,Int)
-- Player = Square
isValidMove :: Board -> Move -> Bool 
isValidMove board (c,r) 
    | not (isMoveInBounds (c,r)) = False
    | otherwise = isColEmpty (board !! c) r 
     
