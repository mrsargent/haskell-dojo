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
printLogo = readFile _LOGO_PATH_ >>= putStrLn   


-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player   
firstPlayer = _RANDOM_BOOL_ >>= \result -> return (getFirstPlayer result)

-- Q#04

getMove :: Board -> IO Move  
getMove b  = 
    putStrLn "Enter a move" >>
    getLine >>= \input ->
    let move = stringToMove input 
    in if isValidMove b move then
            return move 
        else 
            putStrLn "Invalid move bitch. Try again" >>
            getMove b     

-- Q#05

play :: Board -> Player -> IO ()
play b p = printLogo >> printBoard b >> 
            putStrLn (promptPlayer p) >> 
            getMove b >>= \move -> 
            let (state, newBoard) = playMove p b move 
            in case state of 
                InProgress -> play newBoard p 
                XWon -> putStrLn "X won bitch"
                OWon -> putStrLn "O won bitch"
                Tie -> putStrLn "It's a tie bitch"
      
-- Q#06

runTTT :: IO ()
runTTT = do  
    putStrLn "Not implemented... yet!"
    playDo _EMPTY_BOARD_ =<< firstPlayer 


-- Q#07

printLogoDo :: IO () 
printLogoDo = do 
    contents <- readFile _LOGO_PATH_ 
    putStrLn contents 

-- Q#08

firstPlayerDo :: IO Player   
firstPlayerDo = do 
    result <- _RANDOM_BOOL_
    return (getFirstPlayer result) 

-- Q#09

getMoveDo :: Board -> IO Move  
getMoveDo b  = do 
    putStrLn "Enter a move"
    input <- getLine 
    let move = stringToMove input 
    if isValidMove b move then
        return move 
    else do 
        putStrLn "Invalid move bitch. Try again"
        getMove b 

-- Q#10

playDo :: Board -> Player -> IO ()
playDo b p = do 
    printLogoDo
    printBoard b 
    putStrLn $ promptPlayer p
    move <- getMoveDo b 
    let (state, newBoard) = playMove p b move 
    case state of 
        InProgress -> playDo newBoard p 
        XWon -> putStrLn "X won bitch"
        OWon -> putStrLn "O won bitch"
        Tie -> putStrLn "It's a tie bitch"