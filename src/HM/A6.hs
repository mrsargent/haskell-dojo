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
{- revealLetters :: Move -> Secret -> Guess -> Guess 
revealLetters m s g = do 
    let len = length s 
        str = replicate len '_'
    putStrLn $ str 
    putStrLn "Take a guess"
    gue <- getLine -}
    
    -- Function to reveal letters in the guess
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess =
    let revealFunc x y = if y == move then y else x
    in zipWith revealFunc guess secret

-- Example usage
main :: Move -> Secret -> Guess -> IO ()
main move secret guess = do
    putStr "Enter a letter you think is in the secret word: "
    m <- getLine
    let move = head m 
        s = "bitch"
        updatedGuess = zipWith (\x y -> if y == move then y else x) s guess    
    putStrLn $ "Secret: " ++ secret
    putStrLn $ "Initial Guess: " ++ guess
    putStrLn $ "Move: " ++ [move]
    putStrLn $ "Updated Guess: " ++ updatedGuess
    if '_' `elem` updatedGuess then 
        putStrLn "keep guessing bitch"
        main '_' s updatedGuess 
    else
        return updatedGuess





    

-- Q#06

updateChances = undefined

-- Q#07

setSecret = undefined

