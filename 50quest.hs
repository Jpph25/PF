-- // 50 QUESTÕES \\

--   1.
-- Makes a list from two given limits 
enumFromTO :: Int -> Int -> [Int]
enumFromTO x y
          | x  > y = [ ]
          | x == y = [x]
          | otherwise
           = x : enumFromTO (x + 1) y   


--   2. 
-- Makes a list from two given limits with the same gap 
enumFromThenTO :: Int -> Int -> Int -> [Int] 
enumFromThenTO x y z 
               | x > z && r >= 0 = []
               | x < z && r <= 0 = []
               | otherwise 
                = x : enumFromThenTO y (y + r) z  
          where
                r = y - x         

--   3. 
-- Takes two list and make them one 
twolToOnel :: [a] -> [a] -> [a] 
twolToOnel  [] [] 
          = [] 
twolToOnel  [] l 
          = l
twolToOnel  l []
          = l 
twolToOnel (h1:t1) (h2:t2) 
          = h1: twolToOnel t1 (h2:t2) 

--   4. 
-- Finds the element of a list in a given position (1st element is in position 0)
position :: [a] -> Int -> a 
position [] x
        = error "Invalid list" 
position (h1:t1) x 
        | x  > 1 - length (h1:t1) = error "Invalid position" 
        | x == 0 = h1 
        | otherwise 
         = position t1 (x - 1) 

--   5. 
-- When given a list revertes the order of the elements 
revert :: [a] -> [a]
revert []
      = []  
revert l
      = last l : revert (init l)   

--   6. 
-- Makes a list with the first n elements of a list 
fstTake :: Int -> [a] -> [a]
fstTake x []
       = [] 
fstTake x (h:t) 
       | length (h:t) <= x = h:t  
       | otherwise 
        = fstTake x (init (h:t)) 

--   7.
-- Makes a list where a given number of elementes is taken from the beginning 
lstTake :: Int -> [a] -> [a]
lstTake x []
       = [] 
lstTake x (h:t)
       | length (h:t) < x = [] 
       | x == 0 = h:t  
       | otherwise 
        = lstTake (x -1) t 

--   8. 
-- Make a list of pairs from two lists 
pairs :: [a] -> [b] -> [(a,b)] 
pairs  [] _ 
     = []
pairs  _ []
     = [] 
pairs  (h1:t1) (h2:t2) 
     = (h1,h2) : pairs t1 t2 

--   9.
-- Makes a kist of the same number with a given length 
rep :: Int -> a -> [a] 
rep 0 x 
   = []
rep l x 
   = x : rep (l - 1) x 

--   10.
-- Makes a list that puts a number in the midle of all elements of a given list 
intspace :: a -> [a] -> [a]
intspace x []
        = [] 
intspace x (h:t)
        = h:x : intspace x t  