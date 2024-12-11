-- // FICHA 5 \\ -- 

import Data.List 
import Graphics.Gloss (dim)

--   1. 

-- a) 
-- Test if a given predicate is true to any element of a list 

truePrd :: (a -> Bool) -> [a] -> Bool 
truePrd f [] 
       = False 
truePrd f (h:t) 
       | f h == True = True 
       | otherwise 
        = truePrd f t 

-- b) 
-- Combines the elements of two list using a given function 

combWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
combWith f [] [] 
        = [] 
combWith f [] _  
        = []
combWith f  _ [] 
        = [] 
combWith f (h1:t1) (h2:t2) 
        = f h1 h2 : combWith f t1 t2 

-- c) 
-- Determine the first elementes of a list that satisfy a predicate 

dertWhile :: (a -> Bool) -> [a] -> [a] 
dertWhile f [] 
         =  [] 
dertWhile f (h:t) 
         | f h == True = h : dertWhile f t
         | otherwise
          = []  

-- d) 
-- Eliminatesthe first elementes of a list that satisfy a predicate 

elimWhile :: (a -> Bool) -> [a] -> [a] 
elimWhile f []
         =  [] 
elimWhile f (h:t) 
         | f h == True = elimWhile f t 
         | otherwise 
          = h:t  

-- e) 
-- Calclates the last tow functions at the same time 

divWhile :: (a -> Bool) -> [a] -> ([a], [a]) 
divWhile f []
        = ([], []) 
divWhile f (h:t) 
        | f h == True = (h : true, false) 
        | otherwise 
         = ([], h:t) 
   where 
         (true, false) = divWhile f t 

-- f) 
-- Deletes the first element that satisfies a given parameter  

elimBy :: (a -> a -> Bool) -> a -> [a] -> [a] 
elimBy f x []
      = [] 
elimBy f x (h:t) 
      | f x h == True = t 
      | otherwise 
       = h : elimBy f x t   

-- g) 
-- Orders a list comparing it by a given parameter 

sortby :: Ord b => (a -> b) -> [a] -> [a] 
sortby f [] 
      =  [] 
sortby f [x] 
      =  [x] 
sortby f (h:x:t) 
      | f h <= f x = h : sortby f (x:t) 
      | otherwise 
       = x : sortby f (h:t) 
         

--   2. 
type Monomial   = (Float, Int)

type Polinomial = [Monomial]

-- a) 
-- Selects the monomials of a determined degree from a polinomial 

selDegree :: Int -> Polinomial -> Polinomial 
selDegree x []
         =  [] 
selDegree x p 
         = filter (\ m -> x == snd m) p 

-- b) 
-- Calculates how many monomials of a certain degree a polinomial has 

count :: Int -> Polinomial -> Int 
count x [] 
     = 0 
count x p 
     = length $ filter (\ m -> x == snd m) p 

-- c) 
-- Calculate the degree of a polinomial 

degree :: Polinomial -> Int 
degree [] 
      = 0 
degree [x] 
      = snd x   
degree p 
--    = foldr max 0 (map snd p) 
      = snd $ last $ sortOn snd p   

-- d) 
-- Calculates the derivate of a polinomial 

deriv :: Polinomial -> Polinomial 
deriv  []
     = []
deriv p 
     = map (\ (x,d) -> (x * fromIntegral d, d - 1)) p  

-- e) 
-- Calculates the value of the polinomial to a given value 

calc :: Float -> Polinomial -> Float 
calc _ [] 
    = 0 
calc x p 
    = sum $ map (\ (c,d) -> (c * x) ^ d) p 

-- f) 
-- Takes from the polinomial all the monomial with coefficient zero 

simp :: Polinomial -> Polinomial 
simp  [] 
    = []  
simp p 
    = dropWhile (\ (_ , d) -> d == 0) $ sortOn snd p  

-- g) 
-- Multiples a monomial and a polinomial 

mult :: Monomial -> Polinomial -> Polinomial 
mult _ [] 
    =  [] 
mult (c,d) p 
    = map (\ (x,y) -> (x * c, d + y)) p   

-- h) 
-- Orders the polinomial by the degree of the monimial 
-- from the biggest to the smalest 

ordDeg :: Polinomial -> Polinomial 
ordDeg p 
      = sortOn snd p 

-- i) 
-- Simplifies the polinomial suming all monomials of the same degree 

normal :: Polinomial -> Polinomial 
normal p 
      = map     (\ m -> (sum (map fst m), snd (head m)))   
      $ groupBy (\ (_ , d1) (_ , d2) -> d1 == d2)    
      $ sortBy  (\ (_ , d1) (_ , d2) -> compare d1 d2) p 

-- j) 
-- Sums two polinomials 

sumPol :: Polinomial -> Polinomial -> Polinomial 
sumPol p1 p2 
      = map     (\ m -> (sum (map fst m), snd (head m))) 
      $ groupBy (\ (_ , d1) (_ , d2) -> d1 == d2) 
      $ sortBy  (\ (_ , d1) (_ , d2) -> compare d1 d2) (p1 ++ p2)

-- k) 
-- Calculates the porduct of two polinomaials 

prdctPol :: Polinomial -> Polinomial -> Polinomial
prdctPol p []
        = []
prdctPol p (h:t)  
        = prdct p h ++ prdctPol p t  
    where 
          prdct l (x,y)  
               = map (\ (c,d) -> (c * x, d + y)) l    

-- j) 
-- Verifies if two polinomials are equivalent 

equiv :: Polinomial -> Polinomial -> Bool 
equiv p1 p2 
     = normal (simp p1) == normal (simp p2) 


--   3. 

type Mat a = [[a]] 

-- a) 
-- Verifies if a matrix is valid  

dimOk :: Mat a -> Bool 
dimOk (row : rows) 
     = all (\ l -> length l == length row ) rows     

-- b) 
-- Calculates the dimension of a matrix 

dimMat :: Mat a -> (Int, Int) 
dimMat  []
      = (0, 0)
dimMat  (row : rows) 
      | dimOk (row : rows) 
       = (length (row : rows), length row)  
      | otherwise 
       = error "Invalid Matrix"  

-- c) 
-- Sums two matrix 

addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat (row1 : rows1) (row2 : rows2)  
      |  dimMat (row1 : rows1) == dimMat (row2 : rows2) 
       = map (\ (x,y) -> x + y) (zip row1 row2) : addMat rows1 rows2
      | otherwise 
       = error "Matrices of different types" 

-- d) 
-- Transpose of a matrix 

transpos :: Mat a -> Mat a 
transpos  [] 
        = [] 
transpos (row : rows) 
        = reverse $ map reverse (row : rows)  

-- e) 
-- Multiplies two matrices 

multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat m1 m2  
       | snd (dimMat m1) == fst (dimMat m2)  
        = [[sum $ zipWith (*) row col | col <- transpos m2] | row <- m1] 
       | otherwise 
        = error "Matrices can not be multiplied"     

-- f) 
-- Combines two matrices using a given parameter 

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c 
zipWMat f m1 m2 
       | dimMat m1 == dimMat m2 
        = [zipWith f row1 row2 | (row1, row2) <- zip m1 m2]  
       | otherwise
        = error "Matrices of deifferent types" 

-- g) 
-- Tests if a squered Matrix is upper triangular 

triSup :: (Num a, Eq a) => Mat a -> Bool 
triSup (row : rows) 
      | fst (dimMat (row : rows)) == snd (dimMat (row : rows)) &&
        nZeros (row : rows)  
       = True 
      | otherwise 
       = False  
 where 
       nZeros :: (Num a, Eq a) =>  Mat a -> Bool 
       nZeros m   
             |   [length (dropWhile (\ x -> x == 0) row) | row <- m] 
              == reverse [1 .. length m]  
              =  True 
             | otherwise 
              =  False

-- h) 
-- Rotates the matrix 90 degrees to the left 

rotateLeft :: Mat a -> Mat a 
rotateLeft m  
          = reverse (transpose m) 