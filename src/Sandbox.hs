module Sandbox where
import Debug.Trace
import Data.List
import Data.Char


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
