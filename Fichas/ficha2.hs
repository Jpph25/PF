--   // FICHA 2 \\

import Data.Char (ord, chr) 

-- 1. 

--  a)
funA :: [Double] -> Double
funA [] 
    = 0
funA (y:ys) 
    = y^2 + (funA ys)

{- 
  funA [2,3,5,1]
= funA (2:[3,5,1])
= 2^2 + funA (3:[5,1])
= 2^2 + 3^3 + funA (5:[1])
= 2^2 + 3^2 + 5^2 + funA (1:[])
= 2^2 + 3^2 + 5^2 + 1^2 + funA ([])
= 2^2 + 3^2 + 5^2 + 1^2 + 0
= 39  
-} 

--  b) 
funB :: [Int] -> [Int]
funB [] 
    = []
funB (h:t) 
    = if   (mod h 2)==0 
      then h : (funB t)
      else (funB t)

{-
  funB [8,5,12]
= funB (8:[5,12])
= 8 : funB (5:[12])
= 8 : funB (12:[])
= 8 : 12 : funB ([])
= 8 : 12 : [] 
= [8,12]
-}

--  c)
funC (x:y:t) = funC t
funC [x]
    = [x]
funC [] 
    = []

{-
  funC [1,2,3,4,5]
= funC (1:[2,3,4,5])
= funC (1:2:[3,4,5])
= func [3,4,5]
= funC (3:4:[5])
= funC [5]
= [5]
-}

--  d) 
funD l = g [] l
g acc [] 
     = acc
g acc (h:t) 
     = g (h:acc) t

{-
  funD "otrec"
= g [] ('o':"trec")
= g ("o":[]) "trec" 
= g ('o':[]) ('t':"rec")
= g ('t':'o':[]) "rec"
= g ('t':'o':[]) ('r':"ec")
= g ('r':'t':'o':[]) "ec"
= g ('r':'t':'o':[]) (e:"c")
= g ('e':'r':'t':'o':[]) "c"
= g ('e':'r':'t':'o':[]) ('c':[])
= g ('c':'e':'r':'t':'o':[]) []
= ('c':'e':'r':'t':'o':[])
= "certo"
-}


-- 2.

--   a)
-- Gives a list that where every elemente is doubled from the original 
doubles :: [Float] -> [Float]
doubles  [] 
       = []
doubles  (h:t) 
       = (h * 2) : doubles t  

--   b) 
-- Calculates how many times a character appers in a word  
numOccurs :: Char -> String -> Int
numOccurs  a [] 
         = 0
numOccurs  a (h:t) 
         = if   a == h
           then 1 + numOccurs a t
           else numOccurs a t 

--   c) 
-- Tests if a given list is only composed by positive numbers  
positive :: [Int] -> Bool 
positive  [] 
         = True
positive  (h:t) 
         = if   h >= 0 
           then positive t  
           else False  

--   d)
-- When given a list gives a same list without all negative numbers
onlyPos :: [Int] -> [Int] 
onlyPos  [] 
       = []
onlyPos (h:t) 
       = if   h >= 0
         then h : onlyPos t 
         else onlyPos t  

--   e)
-- Adds all negative numbers from a list  
sumNeg :: [Int] -> Int
sumNeg  [] 
       = 0
sumNeg  (h:t) 
       = if   h <= 0 
         then h + sumNeg t 
         else sumNeg t 

--   f)
-- Gives a list of the three last elements of a list 
thrlast :: [a] -> [a] 
thrlast (h:t) 
       = if   (length (h:t)) <= 3 
         then (h:t)
         else thrlast (tail (h:t))   

--   g)
-- Gives a list of the second element of all pairs of a given list
seconds :: [(a,b)] -> [b] 
seconds   [] 
        = []
seconds   (h:t) 
        = (snd h) : seconds t

--   h)
-- Tests if a given element is present as the first element of a list of pairs 
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 
nosPrimeiros  a [] 
            = False 
nosPrimeiros  a (h:t) 
            = if   a == (fst h) 
              then True 
              else nosPrimeiros a t 

--   j)
-- Gives a triple of the sum of the componentes of a list of triples 
sumTriples :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
sumTriples  [] 
          = (0,0,0)
sumTriples  ((x,y,z):t) 
          = let (sx,sy,sz) = sumTriples t 
            in  (sx+x, sy+y, sz+z) 


-- 3.

--   a) 
