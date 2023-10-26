module Sandbox where
import Debug.Trace


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
