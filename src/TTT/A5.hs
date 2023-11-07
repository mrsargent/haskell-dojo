module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4
import System.IO
import GHC.Read (readField)

-- Q#01
printBoard :: Board -> IO () 
printBoard b = putStrLn (formatBoard b) 

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO () 
printLogo = do 
    contents <- readFile _LOGO_PATH_ 
    putStrLn contents  


-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player   
firstPlayer = do 
    result <- _RANDOM_BOOL_
    return (getFirstPlayer result) 


-- Q#04

getMove :: Board -> IO Move  
getMove b  = do 
    putStrLn "Enter a move"
    input <- getLine 
    let move = stringToMove input 
    if isValidMove b move then
        return move 
    else do 
        putStrLn "Invalid move bitch. Try again"
        getMove b 
            

-- Q#05

play :: Board -> Player -> IO ()
play b p = do 
    printLogo
    printBoard b 
    putStrLn $ promptPlayer p
    move <- getMove b 
    let (state, newBoard) = playMove p b move 
    case state of 
        InProgress -> play newBoard p 
        XWon -> putStrLn "X won bitch"
        OWon -> putStrLn "O won bitch"
        Tie -> putStrLn "It's a tie bitch"
      



-- Q#06

runTTT :: IO ()
runTTT = putStrLn "Not implemented... yet!"

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined