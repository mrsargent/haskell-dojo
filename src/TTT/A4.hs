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

-- Q#03

dropFirstCol' :: Board -> Board 
dropFirstCol' = map tail 

-- Q#04

dropLastCol' :: Board -> Board 
dropLastCol' = map init 

--Q#05

formatRows' :: [Row] -> [String]
formatRows' xs = map (\x -> formatLine (showSquares' x)) xs 

-- Q#06
isWinningLine_ :: Player -> Line -> Bool 
isWinningLine_ _ [] = False 
isWinningLine_ p l  = 
    let filteredList = filter (\x -> x == p) l 
    in length filteredList == length l

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

getGameState :: Board -> GameState
getGameState b
  | hasWon b X  = XWon 
  | hasWon b O  = OWon
  | isTie b     = Tie
  | otherwise   = InProgress
  where 
    isTie :: Board -> Bool 
    isTie = all (notElem Neither)    

playMove :: Player -> Board -> Move -> (GameState,Board)
playMove p b (x,y) =
  let newBoard = putSquare p b (x,y)
      state = getGameState newBoard
  in (state, newBoard)

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices = map combineToString . zipWith (\x y -> (x,y)) ['A'..] 
  where
    combineToString :: (Char,String) -> String 
    combineToString (a,b) = a:b 

-- Q#11

formatBoard :: Board -> String 
--formatBoard = (++) _HEADER_ . unlines . prependRowIndices . formatRows'
formatBoard = unlines . ((++) [str] . prependRowIndices . formatRows')
  where
    str = " " ++ _HEADER_
