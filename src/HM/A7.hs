module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)

-- Q#01
data Game = Game { 
      secretWord :: String
    , currentGuess :: String
    , guessedMove :: [Char]
    , chancesRemaining :: Int     
    } 

-- Q#02
repeatedMove :: Move -> Game -> Bool 
repeatedMove m g = m `elem` guessedMove g 

-- Q#03
makeGame :: Secret -> Game 
makeGame secret = Game {
   secretWord = map toUpper secret
  ,currentGuess = replicate (length secret) '_'
  ,guessedMove = []
  ,chancesRemaining = _CHANCES_
}

-- Q#04
updateGame :: Move -> Game -> Game 
updateGame m g = Game {  
   currentGuess = revealLetters m (secretWord g) (currentGuess g)
  ,guessedMove = m : guessedMove g
  ,chancesRemaining = updateChances m (secretWord g) (chancesRemaining g) 
  ,secretWord = secretWord g 
}

-- Q#05

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]

instance Show Game where
  show game = showGameHelper (secretWord game) (guessedMove game) (chancesRemaining game)

-- Q#06

instance Show GameException where
  show InvalidChars = "Inavlid Chars in guess"
  show InvalidLength = "Must have a length between " ++ lb ++ " and " ++ ub
    where 
      lb = show $ fst _LENGTH_ 
      ub = show $ snd _LENGTH_ 
  show NotInDict = "Word guessed is not in dictionary"
  show InvalidMove = "invalid move. Please enter valid move"
  show RepeatMove = "Repeated move"
  show GameOver = "game over bitch"


-- Q#07
toMaybe :: Bool -> a -> Maybe a 
toMaybe x a 
  | x         = Just a 
  | otherwise = Nothing 

-- Q#08

validateSecret = undefined

-- Q#09

hasValidChars = undefined

isValidLength = undefined

isInDict = undefined

-- Q#10

validateNoDict = undefined

validateWithDict = undefined

-- Q#11

processTurn = undefined