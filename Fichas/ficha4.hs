-- // FICHA 4 \\ -- 

import Data.Char
import Data.List 

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

-- Optimization of fromDigits: using acumulaters
fromDigitsOpt :: [Int] -> Int 
fromDigitsOpt []
             = 0 
fromDigitsOpt l 
             = acc 0 l 
        where 
               acc  n []
                  = n
               acc  n (h:t) 
                  = acc (n * 10 + h) t      


--   5. 
-- Calculates the biggest possible sum of the subsets of a list 
maxSumInit :: (Num a, Ord a) => [a] -> a 
maxSumInit l 
          = maximum [sum m | m <- inits l]  

-- Optimization of maxSumInit: using an accumulator    
maxSumInit'' :: (Num a, Ord a) => [a] -> a
maxSumInit'' l 
            = acc 0 0 l 
        where 
              acc m n [] 
                 = m 
              acc m n (h:t) 
                 | n + h > m = acc (n + h) (n + h) t 
                 | otherwise 
                  = acc m (n + h) t

{-
-- Not well optimized, but works 
maxSumInit' :: (Num a, Ord a) => [a] -> a
maxSumInit' l 
           = maximum (acc 0 (inits l))  
       where  
            acc n [] 
               = [n]  
            acc n (h:t)
               = (n + sum h) : acc 0 t
-}


--   6. 
-- Calculates a determined element of the Fibonacci squence 
fib :: Int -> Int
fib 0 
   = 0
fib 1 
   = 1
fib n 
   = fib (n-1) + fib (n-2)

-- Optimization of fib: using two accumulators
fib' :: Int -> Int 
fib'   0 
     = 0 
fib'   1
     = 1 
fib'   n 
     = acc 0 1 n 
 where   
       acc x y 0 
          = x 
       acc x y n 
          = acc y (x + y) (n - 1) 


--   7. 
-- Tranformes a number into a text 
inToStr :: Integer -> String 
inToStr n 
       | n <  0 = "-" ++ inToStr (-n)
       | n == 0 = "0"   
       | otherwise
        = acc "" n 
   where
         acc    l 0 
              = l 
         acc    l n 
              = acc (digitToChar (mod n 10): l) (div n 10)  
          where
                digitToChar n 
                           = toEnum (fromEnum '0' + fromEnum n)    


--   8.

-- a) 
-- [2,3,4,6,8,9,10,12,14,15,16,18,20] 

-- b) 
-- [x | x <- [2,4,6,8,10,12,14,16,18,20] , mod x 3 == 0]
-- [6,12,18]

-- c) 
-- [(0,20) ,(1,19) ,(2,18)  ,(3,17) ,(4,16) ,(5,15) ,(6,14) ,(7,13), 
--  (8,12) ,(9,11) ,(10,10) ,(11,9) ,(12,8) ,(13,7) ,(14,6) ,(15,5),
--  (16,4) ,(17,3) ,(18,2)  ,(19,1) ,(20,0)                         ]

-- d) 
-- [sum [y | y <- [1 .. [1..10]] odd y]
-- [sum [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],
--  [1,2,3,4,5,6],[1,2,3,4,5,6,7],[1,2,3,4,5,6,7,8],
--  [1,2,3,4,5,6,7,8,9],[1,2,3,4,5,6,7,8,9,10]]]
-- [sum [[1],[1],[1,3],[1,3],[1,3,5],[1,3,5],[1,3,5,7],[1,3,5,7],
--  [1,3,5,7,9],[1,3,5,9]]]
-- [1,1,4,4,9,9,16,16,25,25] 


--   9. 

-- a) 
-- [ 2^x | x <- [1 .. 10] ] 

-- b) 
-- [ zip [1 .. 5] [5, 4 .. 1] ] 

-- c) 
-- [ [1 .. x] | x <- [1 .. 5] ] 

-- d) 
-- [ take x [1, 1 ..] | x <- [1 .. 5] ]   

-- e)
-- Factorial function 

factorial :: Int -> Int 
factorial 0 = 1 
factorial x 
         = x * factorial (x - 1)  

-- [ factorial x | x <- [1 .. 6] ]
         