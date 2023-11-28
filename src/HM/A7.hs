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
validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret 
validateSecret pred exception secret
  | pred secret = Right secret
  | otherwise   = Left exception 


-- Q#09

hasValidChars :: Secret -> GameException -> Either GameException Secret  
hasValidChars secret except = validateSecret (\x -> all isAlpha x) except secret 

isValidLength :: Secret -> GameException -> Either GameException Secret
isValidLength secret except = validateSecret (\x -> lengthInRange x) except secret 

isInDict :: Dictionary -> Secret -> GameException -> Either GameException Secret
isInDict dict secret except = validateSecret (\x -> map toLower x `elem` dict) except secret 

-- Q#10

validateNoDict :: Secret -> Either GameException Secret 
validateNoDict secret = do
  c <- hasValidChars secret InvalidChars
  l <- isValidLength secret InvalidLength 
  return secret 

validateNoDict' :: Secret -> Either GameException Secret 
validateNoDict' secret = 
  case hasValidChars secret InvalidChars of 
    Left err -> Left err 
    Right validSecret ->
      case isValidLength secret InvalidLength of
        Left err -> Left err 
        Right validLength -> Right secret -- return secret would have also have worked 


validateWithDict :: Secret -> Dictionary -> Either GameException Secret 
validateWithDict secret dict = do 
  c <- validateNoDict secret 
  d <- isInDict dict secret NotInDict 
  return secret 


-- Q#11

processTurn :: Move -> Game -> Either GameException Game 
processTurn move game
  | repeatedMove move game            = Left RepeatMove 
  | invalidMove move                  = Left InvalidMove
  | chancesRemaining updatedGame == 0 = Left GameOver
  | otherwise                         = Right updatedGame 
    where 
      updatedGame = updateGame move game  

  