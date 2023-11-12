module HM.A6 where

import Data.Char (isAlpha)
import HM.Provided
import System.IO

-- Q#01
type Chances = Int
type Guess = String 
type Move = Char 
type Secret = String 
type Dictionary = [String] 

-- Q#02
data GameException = InvalidChars | InvalidLength | NotInDict | InvalidMove | RepeatMove | GameOver 

-- Q#03

lengthInRange :: Secret -> Bool 
lengthInRange s =
    let (min,max) = _LENGTH_
        secretLength = length s  
    in secretLength <= max && secretLength >= min
   

-- Q#04
invalidMove :: Move -> Bool 
invalidMove c 
    | isAlpha c     = True
    | otherwise     = False 

-- Q#05
    -- Function to reveal letters in the guess
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess =
    let revealFunc x y = if y == move then y else x
    in zipWith revealFunc guess secret

playGame :: Secret -> Guess -> IO () 
playGame secret guess = do 
    putStrLn $ "Here is your current guesses: " ++ guess 
    putStr "Enter a character you think is in the secret phrase: "
    move <- getLine

    let m = head move
        updatedGuess = revealLetters m secret guess 

    if updatedGuess == secret then
        putStrLn $ "Congrats you won. You guessed the secret word: " ++ secret
    else
        playGame secret updatedGuess 


-- Example usage
main :: IO ()
main = do
    let secret = "haskell"
        iniitialGuess = replicate (length secret) '_'
    putStrLn "Let's play a guessing game"
    playGame secret iniitialGuess    

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances 
updateChances move secret chance = 
    if move `elem` secret then
        chance 
    else
        chance - 1 

-- Q#07
setSecret :: IO ()
setSecret = do 
    putStr "Enter a secret phrase you fucking pussy:  "
    showInput True 
    secret <- getLine 
    putStrLn $ "You entered the word " ++ secret
    _SPACE_
    putStr "Enter another secret phrase you fucking pussy:  "
    showInput False
    secret <- getLine    
    putStrLn $ "You entered the word " ++ secret



