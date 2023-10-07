module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
--convertRowIndex a = fromEnum (toUpper a) - 65  
convertRowIndex = (-) 65 . fromEnum . toUpper

-- Q#04
_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: String
_SEP_ = "_|_"

-- Q#06

data Square = X | O | Neither deriving (Show, Eq) 

-- Q#07

data GameState = XWon | OWon | Tie | InProgress deriving Show 

-- Q#08
type Player = Square
type Row = Square
type Line = Square
type Board = Row 
type Move = (Int,Int)
-- Q#09

getFirstPlayer :: Bool -> Player 
getFirstPlayer a = if a then X else O 

getFirstPlayer_ a 
    | a = X
    | otherwise = O

    
-- Q#10
showGameState :: GameState -> String 
showGameState gs = case gs of 
    XWon -> "X Won"
    OWon -> "O Won"
    Tie -> "It's a tie"
    InProgress -> "In progress baby!"


-- Q#11
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer Neither = Neither


-- Q#12
showSquare :: Square -> String 
showSquare s 
    | s == X = "X"
    | s == O = "O"
    | s == Neither = "_"
    | otherwise = "nada"

