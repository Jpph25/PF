-- // FICHA 4 \\ -- 

import Data.Char
import Data.List (inits)

--   1. 
-- When given a string, gives back a pair where the first element only has
-- only the character and the second element only the digits 
digitAlpha :: String -> (String,String) 
digitAlpha []
          = ([],[])  
digitAlpha (h:t) 
          | isDigit h = (   ltrs, h: digt) 
          | isAlpha h = (h: ltrs,    digt) 
          | otherwise 
           = (ltrs,digt)
     where (ltrs,digt) = digitAlpha t   


--   2.
-- Counts how many negative, zeros and posite numbers there are in a list of numbers
negzpos :: [Int] -> (Int,Int,Int) 
negzpos (h:t) 
       | h  < 0 = (1 + neg,     zero,     pst)
       | h == 0 = (    neg, 1 + zero,     pst)
       | h  > 0 = (    neg,     zero, 1 + pst) 
  where (neg,zero,pst) = negzpos t 

--   3. 
-- Calculates the division and the integer remainder at the same time 
divmod :: Integral a => a -> a -> (a,a) 
divmod div dvr        
      | div < dvr = (0 ,div) 
      | otherwise 
       = (q + 1, r)  
 where (q,r) = divmod (div - dvr) dvr      

--   Separated function 
-- Division  
mydiv :: Int -> Int -> Int  
mydiv dvo dvr 
     | dvo  < dvr = 0 
     | otherwise 
      = 1 + mydiv (dvo - dvr) dvr 
-- Integer remainder 
mymod :: Int -> Int -> Int 
mymod dvo dvr 
     | dvo  < dvr = dvo 
     | otherwise
      = mymod (dvo - dvr) dvr 


--   4. 
-- Tranforms a list of single digits into one number 
fromDigits :: [Int] -> Int
fromDigits [] 
          = 0
fromDigits (h:t) 
          = h * 10 ^ length t + fromDigits t

-- Optimization if the function using acumulaters
fromDigitsOpt :: [Int] -> Int 
fromDigitsOpt []
             = 0 
fromDigitsOpt l 
             = acc 0 l 
        where 
              acc n []
                 = n
              acc n (h:t) 
                 = acc (n * 10 + h) t      
















-- Optimization of fromDigits: using an accumulator  
-- Transforms a list of numbers into only one number  

fromDigits' :: [Int] -> Int 
fromDigits' l 
           = acc 0 l 
      where  acc n []    = n 
             acc n (h:t) = acc (n * 10 + h) t   

--   5. 

maxSumInit' :: (Num a, Ord a) => [a] -> a
maxSumInit' l 
          = maximum [sum m | m <- inits l] 

-- Optimization of maxSumInit: using an accumulator 
-- Gives the bigger sum of the inits of a list 

maxSumInit'' :: (Num a, Ord a) => [a] -> a 
maxSumInit'' l  
           = acc 0 0 l 
      where acc m s [] = m 
            acc m s (h:t) 
               | h + s > m = acc (h + s) (h + s) t 
               | otherwise 
                = acc m (h + s) t 

--   6. 

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n 
   = fib' (n - 1) + fib' (n - 2) 

-- Optimization of fib: using two accumulators 
-- Gives the sum of the numbers of the fibonacci sequence to a certain element

fib'' :: Int -> Int 
fib'' 0 = 0 
fib'' 1 = 1 
fib'' n 
     = acc 1  0  n 
 where acc n2 n1 0 = n1 
       acc n2 n1 n = acc (n2 + n1) n2 (n - 1)  
