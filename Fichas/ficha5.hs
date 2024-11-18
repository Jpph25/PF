-- // FICHA 5 \\ -- 

-- 1. 

--   a) 
-- Sees if for a given list and a function 
-- one element of the list is valid for the function 
anyElemT :: (a -> Bool) -> [a] -> Bool 
anyElemT f [] 
        = False  
anyElemT f (h:t) 
        | f h == True  = True  
        | f h == False = anyElemT f t 

--   b) 
-- Given two lists makes a gu«iven operation between the two 
oprtList :: (a -> b -> c) -> [a] -> [b] -> [c] 
oprtList f _ []
        =  [] 
oprtList f [] _         
        =  [] 
oprtList  f (h1:t1) (h2:t2) 
        = f h1 h2 : oprtList f t1 t2 

--   c) 
-- Gives the first elements that are valid in a given function 
gValidF1 :: (a -> Bool) -> [a] -> [a] 
gValidF1 f [] 
        =  [] 
gValidF1 f (h:t) 
       | f h == True = h : gValidF1 f t 
       | otherwise 
        = []

--   d) 
-- Takes the first elements that are valid in a given function from a list 
tValidF1 :: (a -> Bool) -> [a] -> [a] 
tValidF1 f []
        =  []
tValidF1 f (h:t)   
        | f h == True  = tValidF1 f t 
        | otherwise 
         = h:t 

--   e) 
-- Gives a pair of the results of the previous functions 
tgValidF1 :: (a -> Bool) -> [a] -> ([a],[a]) 
tgValidF1 f [] 
         =  ([],[])  
tgValidF1 f (h:t) 
         | f h == True = (h : true, false)
         | otherwise 
          = ([] , h:t) 
    where 
         (true,false) = tgValidF1 f t      

--   f) 
-- Deletes the first element of a list that satisfies a given parameter 
delFstV :: (a -> a -> Bool) -> a -> [a] -> [a] 
delFstV _ _ []
       = []
delFstV f x (h:t) 
       | f x h == True = t 
       | otherwise 
        = h : delFstV f x t 

--   h) 
-- Orders a list coparing a taken element from it 
delOrd :: Ord b => (a -> b) -> [a] -> [a] 
delOrd f [] 
      =  []
delOrd f [x]
      =  [x] 
delOrd f (h:x:t)  
      | f h <= f x = h : delOrd f (x:t) 
      | otherwise 
       = x : delOrd f (h:t) 


-- 2. 

type Polinomial = [Monomial]

type Monomial = (Float,Int)

--   a) 
-- Selectes the monomials of a chosen degree from a polinomial 
selDegre :: Int -> Polinomial -> Polinomial 
selDegre 