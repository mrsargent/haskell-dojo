module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01
showInts' :: [Int] -> [String]
showInts' xs = map show xs

_HEADER_ :: String
_HEADER_ =  let str = showInts' _RANGE_
            in formatLine str 

-- Q#02

showSquares' :: [Square] -> [String]
showSquares'  = map show
-- showSquare xs = map show xs   // explicit implementation

-- Q#03

dropFirstCol' :: Board -> Board 
--dropFirstCol []=[]
--dropFirstCol (x:xs) = tail x : dropFirstCol xs  
dropFirstCol' = map tail 

-- Q#04

dropLastCol' :: Board -> Board 
--dropLastCol' [] = []
--dropLastCol' (x:xs) = init x : dropLastCol xs
dropLastCol' = map init 

--Q#05

formatRows' :: [Row] -> [String]
formatRows' xs = map (\x -> formatLine (showSquares' x)) xs 
--formatRows [] = [] 
--formatRows (x:xs) = formatLine (go x) : formatRows xs 
--    where   
--        go :: Row -> [String]
--        go [] = [] 
--        go y = showSquares y 


-- Q#06
isWinningLine_ :: Player -> Line -> Bool 
isWinningLine_ _ [] = False 
isWinningLine_ p l  = 
    let filteredList = filter (\x -> x == p) l 
    in length filteredList == length l

--isWinningLine :: Player -> Line -> Bool 
--isWinningLine _ [] = False 
--isWinningLine player xs = go False player xs 
  --  where
    --    go :: Bool -> Player -> Line -> Bool 
      --  go True _ [] = True 
      --  go _ p (y:ys) = y == p && go True p ys 
      --  go bool _ [] = bool 


-- Q#07

isWinningLine :: Player -> Line -> Bool 
isWinningLine _ [] = False
isWinningLine p l = foldr (\x acc -> x == p && acc) True l

-- Q#08

hasWon :: Board -> Player -> Bool 
hasWon b p = 
    let tot = getAllLines b -- tot = [Lines]
    in foldr (\x acc -> acc || isWinningLine p x) False tot
     
_X_WIN_ = [ [X, O, O], [O, X, O], [O, O, X]]
_O_WIN_ = [ [O, X, O], [X, X, O], [X, O, O]]
-- Q#09

getGameState = undefined

playMove = undefined

-- Q#10

prependRowIndices = undefined

-- Q#11

formatBoard = undefined