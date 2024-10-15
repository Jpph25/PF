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
infst :: (Eq a) => a -> [(a,b)] -> Bool 
infst  a [] 
     = False 
infst  a (h:t) 
     = if   a == (fst h) 
       then True 
       else infst a t  

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
-- Chooses the algarism from a list of characters 
onlydigits :: [Char] -> [Char]
onlydigits  [] 
          = []
onlydigits (h:t) 
          = if   ord h <= ord '9' && ord h >= ord '0' 
            then h : onlydigits t
            else onlydigits t

--   b)
-- Counts the amount of letters in lowercase in a list
lowercase :: [Char] -> Int 
lowercase  []
         = 0 
lowercase (h:t)
         = if   ord h <= ord 'z' && ord h >= ord 'a'
           then 1 + lowercase t
           else lowercase t    

--   c)
-- Gives a list of the algarism presente on a text
nubs :: String -> [Int] 
nubs  []
    = []
nubs (h:t) 
    = if   ord h <= ord '9' && ord h >= ord '0'
      then ord h - ord '0' : nubs t 
      else nubs t 


-- 4.

type Monomial   = (Float,Int)
type Polynomial = [Monomial]

--   a) 
-- Counts how many monomial of a certain degree there are in a polynomial 
count :: Int -> Polynomial -> Int 
count  g []
     = 0
count  g (h:t)
     = if   snd h == g 
       then 1 + count g t
       else count g t  

--   b)
-- Indicates the degree of the polynomial
degree :: Polynomial -> Int
degree  []
      = 0
degree [x]
      = snd x 
degree (h:x:t) 
      | snd h >= snd x = degree (h:t)  
      | snd h <= snd x = degree (x:t) 

--   c)
-- Selects the monomials of a given degree from a polynomial 
seldegree :: Int -> Polynomial -> Polynomial 
seldegree d []
         = []
seldegree  d (h:t)
         = if   d == snd h 
           then h : seldegree d t
           else seldegree d t  

--   d)

type Polynomial' = [Monomial']
type Monomial'   = (Float,Float) 

-- Calculate the derivative of a polynomial 
deriv :: Polynomial' -> Polynomial' 
deriv  []
     = []
deriv ((n,d):t)
     = (d * n, d - 1) : deriv t 

--   e)
-- Calculates the polynomial to a certain x 
calc :: Float -> Polynomial -> Float 
calc x []
    = 0 
calc x ((n,d):t)
    = ((n*x)^d) + calc x t    

--   f) 
-- Takes from the polynomail the zero degree monomials 
simp :: Polynomial -> Polynomial 
simp  [] 
    = []
simp (h:t) 
    = if   snd h == 0 
      then simp t 
      else h: simp t 

--   g)
-- Calculates the multiplication of a monomial whith a polynomial 
mult :: Monomial -> Polynomial -> Polynomial 
mult (n,d) []
    = [] 
mult (n,d) ((x,y):t) 
    = (n * x, d + y): mult (n,d) t 

--   h)
-- Simplifies the polynomial if he is in order of degrees 
normal :: Polynomial -> Polynomial 
normal  [ ]
      = [ ]
normal  [x] 
      = [x]
normal (h:x:t)
      | snd h == snd x = normal ((fst h + fst x, snd h): t) 
      | otherwise = h:normal (x:t)

--   i) 
-- Sums two polynomials
sump :: Polynomial -> Polynomial -> Polynomial 
sump  [] [] 
    = []
sump [] [x]
    = [x] 
sump [x] []
    = []            
sump (h1:t1) (h2:t2) 
    | snd h1 == snd h2 = (fst h1 + fst h2, snd h1): sump t1 t2  
    | snd h1  < snd h2 = h2: sump (h1:t1) t2 
    | snd h1  > snd h2 = h1: sump t1 (h2:t2)   

--   j) 
-- Multiplies two polynomials 
multp :: Polynomial -> Polynomial -> Polynomial 
multp  []  _   
     = []
multp   _  []
     = []
multp  (h1:t1) (h2:t2) 
     = mult h1 (h2:t2) ++ multp t1 (h2:t2)  

--   k) 
-- Puts the polinomial in order of degrees 
order :: Polynomial -> Polynomial
order  []
     = []
order  [x]
     = [x]
order (h:x:t)
     | snd h >= snd x = h: order (x:t)
     | snd h  < snd x = x: order (h:t) 

--   l)
-- Verifies if one polynonial is equal to another 
equal :: Polynomial -> Polynomial -> Bool 
equal  [] []
     = True  
equal [x] [y]
     = x == y  
equal (h1:t1) (h2:t2)
     | length (h1:t1) /= length (h2:t2) = False  
     | h1 == h2 && equal t1 t2          = True 
     | otherwise                        = False 