module Sandbox where
import Debug.Trace
import Data.List
import Data.Char
import Text.Read


-- recursion examples
factorial :: Int -> Int
factorial 0 = 1 
factorial 1 = 1
factorial n 
    | n > 1 = n * factorial (n-1)
    | otherwise = error "Negetive number"

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' (xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
--sum' xs = foldr (+) 0 xs
-- debug fucntion trace for the sum' function
--sum' (x:xs) = trace ("sum' " ++ show xs) (x + sum' xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 


--iterative recursion approach 
sum_iter :: [Int] -> Int
sum_iter xs = loop 0 xs
    where 
        loop count [] = count
        loop count (h:t) = loop (count + h) t

reverse'' :: [a] -> [a]
reverse'' xs = go [] xs 
    where
        go :: [a] -> [a] -> [a]
        go acc (y:ys) = go (y:acc) ys 
        go acc []     = acc 

--here's the process of how this works
-- reverse'' "star"
-- go [] (s:tar) = go (s:[]) tar
-- go [s] (t:ar) = go (t:[s]) ar
-- go [ts] (a:r) = go (a:[ts]) r
-- go [ats] (r:[]) = go (r:[ats]) []
-- go [rats] []    = rats


-- more iterative recursion with trace
freq :: Char -> String -> Int 
freq c xs = loop 0 xs 
    where
        loop :: Int -> String -> Int 
        loop n []       =  trace (show n) n 
        loop n (y:ys) 
            | c == y    = trace (show n ++ " |" ++ show y) (loop (n+1) ys) 
            | otherwise =  trace (show n ++ " |" ++ show y) (loop n ys) 


---------------------------------------------------------------------------------------------------------------
--notes from higher order functions lecture (10/26/23)
-- filter (\c -> not (elem c "aeiou")) Squirtle
-- result "Sqrtl"


--inductive
sum_ind [] = 0
sum_ind (x:xs) = x + sum_ind xs 

prod_ind [] = 1
prod_ind (x:xs) = x * prod_ind xs 
prod_ind' xs = foldr (\x y -> x * y) 1 xs 


--iterative
sum_iter2 x = loop 0 x where
        loop s [] = s
        loop s (x:xs) = loop (s + x) xs 

--             function   (base) (list)  (ouptut which is same type as base case)
-- foldr :: (a -> b -> b) -> b -> [a] -> b 

-- foldr (\x y -> x + y) 0 [1,2,3,4]
--  1,2,3,4 <-0
--  1 + (2 + ( 3 + (4 + 0)))  you use base in the very first function.  Y Acts like an accumulator 
--      and acuumulator is initalized in the base of foldr
-- run this example to reenforce 
sum'' xs = foldr (\x y -> trace ("x= " ++ show x ++ " | y = " ++ show y) x + y ) 0 xs 
-- result sum'' [1,2,3,4]
-- x= 4 | y = 0
-- x= 3 | y = 4
-- x= 2 | y = 7
-- x= 1 | y = 9
-- 10


--foldl 0 [1,2,3,4]
-- (((0 + 1) + 2) + 3) + 4  starting from the beginning as opposed to the end 


sum_foldl xs = foldl (\a y -> trace ( concat ["a= ", show a, " | h = ", show y]) a + y) 0 xs 

-- foldr (\x y -> x + y) 0 [1,2,3,4]   // the y is the accumulator
-- foldl (\x y -> x + y) 0 [1,2,3,4]   // the x is the accumulator


--remember foldr the y (second element is accum)
freq_foldr :: Char -> String -> Int 
freq_foldr c x = foldr step 0 x 
    where
        step item accum
            | item == c = accum + 1 
            | otherwise = accum 
        

--remember foldr the x (second element is accum)
freq_foldl :: Char -> String -> Int 
freq_foldl c x = foldl step 0 x 
    where
        step accum item 
            | item == c = accum + 1 
            | otherwise = accum 

-------------------------------------------------------------------------------------------------------
--notes from lecture (10/31/23)
--zipwith' :: [a] -> [b] -> [c]
zipwith' x y = zipWith (\a b -> [a,b]) x y 
zipwith'' = zipWith (\a b -> a : b : []) "PQR" "XYZ"
zipwith''' = zipWith (\a b -> [a] ++ [b]) "PQR" "XYZ"


-- (+) ((+) 3 4) 5
-- 3 + 4 + 5

-- ($) apply  , (.) compose
-- reverse ("abc" ++ "def") == reverse $ "abc" ++ "def"

-- double x = x + x
-- square x = x^2 
-- (square . double) 7  or square . double $ 7 

------------------------------------------------------------------------------------------------------
-- notes from lecture (11/2/23)
-- main :: IO ()
-- main = 
--    putStrLn "hello world " >>
--    putStrLn "nice to meet you"
-- result: hello world \n nice to meet you

-- this style is called handing lambda
main = putStrLn "Enter a lowercase phrase" >> 
    getLine >>= \s -> 
    putStrLn "Uppercase version of phrase" >> 
    putStrLn (map toUpper s)   


-- do notation 
main' = do
    putStrLn "Enter a lowercase phrase"
    s <- getLine
    putStrLn "Uppercase version of phrase"
    putStrLn (map toUpper s)



-- notes from lecture (11/14/23)
-- record syntax baby!!!!
data Pokemon = Pokemon {
    pName :: String,
    pId   :: Int,
    pPower :: [String]
}  --deriving Show 
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- ohhhhhhhh  the reason why deriving Show uses a capital S while the show function uses a lower case s is because
-- this derives the type clasa and types are always capitalized (e.g. Float, Int, String, etc)
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data Trainer = Trainer {
    tName :: String,
    tCity :: String 
}


pikachu = Pokemon "Pikachu" 25 ["Electric"]
charmander = Pokemon "Chamander" 4 ["Fire"]
-- map pName [pikachu,charmander]    result: returns the name of both types

data Employee = 
    Manager  {mName :: String, mExpt ::Float}
    | Coder {cName :: String, cExpt ::Float}

getSalary :: Employee -> Float 
getSalary (Manager _ e) = 50 + 12 * e 
getSalary (Coder _ e) = 47 + 9 * e 
-- getSalary (Manager "Bob" 15)   
-- return 230.0 


--if we derive show directly in the pokemon type this is the output
    -- Pokemon {pName = "Pikachu", pId = 25, pPower = ["Electric"]}
--what if we just want to show the pName??? We have to manually derive 

instance Show Pokemon where 
    show p = pName p ++ " " ++ show (pId p)

data Boss = Boss { bName :: String, bExp :: Float}
data Developer = Developer {dName :: String, dExp :: Float, cBoss :: Boss}

class Worker a where 
    getSalary' :: a -> Float 


instance Worker Boss where   
    getSalary' (Boss _ e) = 12 * e 


instance Worker Developer where    
    getSalary' (Developer _ e _) = 10 * e


-- lecture 11/16/23                                                         action     callback  
-- (>>=) takes an action of type IO a then it takes a callback and combines  IO a -> (a -> IO b) -> IO b 
-- (>>=) m a -> (a -> m b) -> m b 


safeDiv :: Int -> Int -> Maybe Int 
safeDiv x 0 = Nothing 
safeDiv x y = Just (div x y)

divThenAdd :: Int -> Int -> Int -> Maybe Int 
divThenAdd x a b = 
    case safeDiv x a of 
        Nothing -> Nothing
        Just v ->
            case safeDiv x b of 
                Nothing -> Nothing
                Just w -> Just (v + w)

divThenAdd' :: Int -> Int -> Int -> Maybe Int 
divThenAdd' x a b = 
    safeDiv x a >>= \v ->
    safeDiv x b >>= \w ->
    Just (v+w)

-- lecture from 11/21
type Name = String
type Id = Int
type Power = [String] 
    
data Pokemon2 = MkPokemon Name Id Power deriving Show 

capFirstLetter :: String -> String 
capFirstLetter [] = []
capFirstLetter (h:t) = toUpper h : t 

nameValProc :: String -> Maybe Name 
nameValProc n 
    | length n < 4 = Nothing  -- ERROR
    | otherwise = Just $ capFirstLetter $ filter isAlphaNum n

idValProc :: String -> Maybe Id 
idValProc id = 
    case readMaybe id of 
        Nothing -> Nothing
        Just v -> if v < 1 then Nothing else Just v 

powerValProc :: [String] -> Maybe Power 
powerValProc power 
    | null power = Nothing 
    | otherwise = Just $ map capFirstLetter power 


--pain in the ass way to extract the maybe out of the function
mkPokemon :: String -> String -> [String] -> Maybe Pokemon2
mkPokemon sName sId sPower =
    case nameValProc sName of 
        Nothing -> Nothing 
        Just name -> 
            case idValProc sId of 
                Nothing -> Nothing
                Just num ->
                    case powerValProc sPower of 
                        Nothing -> Nothing
                        Just powers -> Just $ MkPokemon name num powers  

--much better way to extract the maybe out of the funtion
mkPokemon' :: String -> String -> [String] -> Maybe Pokemon2
mkPokemon' sName sId sPower =
    nameValProc sName   >>= \n ->
    idValProc sId       >>= \i ->
    powerValProc sPower >>= \p ->
    return $ MkPokemon n i p --either return or Just work
    --Just $ MkPokemon n i p 


--much better way to extract the maybe out of the funtion uinsg do notation
mkPokemon'' :: String -> String -> [String] -> Maybe Pokemon2
mkPokemon'' sName sId sPower = do 
    n <- nameValProc sName 
    i <- idValProc sId 
    p <- powerValProc sPower 
    return $ MkPokemon n i p 



-- lecture 11/23/23
-- using Either instead of Maybe
data Error = InvalidName | WrongNumber | EmptyPowers | NegativeNumber deriving Show 

nameValProc' :: String -> Either Error Name 
nameValProc' n 
    | length n < 4 = Left InvalidName 
    | otherwise = Right . capFirstLetter $ filter isAlphaNum n

idValProc' :: String -> Either Error Id 
idValProc' id = 
    case readMaybe id of 
        Nothing -> Left WrongNumber
        Just v -> if v < 1 then Left NegativeNumber else Right v  

powerValProc' :: [String] -> Either Error Power 
powerValProc' power 
    | null power = Left EmptyPowers 
    | otherwise = Right . map capFirstLetter $ power 


mkPokemon''' :: String -> String -> [String] -> Either Error Pokemon2
mkPokemon''' sName sId sPower = do 
    n <- nameValProc' sName 
    i <- idValProc' sId 
    p <- powerValProc' sPower 
    return $ MkPokemon n i p  


-- lecture 12/1/23
-- maybe, either, and IO are all mondads

-- class Monad m where
--      (>>=):: m a -> (a -> m a) -> m b 

--see safeDiv from above.  the signature is Int -> Int -> Maybe Int
-- (safeDiv 12 3) >>= (\x -> return (x+1))
-- fmap (\x -> x + 1) (safeDive 12 3)    !!!! fmap works with monads

-- immutability
-- static typing
-- type system
-- purity

-- Maybe    a
-- Either e a
-- IO       a
-- State s  a
-- Reader r a