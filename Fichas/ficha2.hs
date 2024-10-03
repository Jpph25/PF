-- Ficha 2

-- 1 

--  a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

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
funB [] = []
funB (h:t) = if (mod h 2)==0 
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
funC [x] = [x]
funC [] = []

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
g acc [] = acc
g acc (h:t) = g (h:acc) t

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

-- 2

--  a) 
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h * 2) : dobros t  

--  b) 
numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a (h:t) = if   a == h
                    then 1 + numOcorre a t
                    else numOcorre a t 

--  c) 
positivos :: [Int] -> Bool 
positivos [] = True
positivos (h:t) = if   h >= 0 
                  then positivos t  
                  else False  

--  d) 
soPos :: [Int] -> [Int] 
soPos [] = []
soPos (h:t) = if   h >= 0
              then h : soPos t 
              else soPos t  

--  e) 
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if   h <= 0 
                then h + somaNeg t 
                else somaNeg t 

--  f)
tresUlt :: [a] -> [a] 
tresUlt (h:t) = if   (length (h:t)) <= 3 
                then (h:t)
                else tresUlt (tail (h:t))   

--  g)
segundos :: [(a,b)] -> [b] 
segundos [] = []
segundos (h:t) = (snd h) : segundos t

--  h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 
nosPrimeiros a [] = False 
nosPrimeiros a (h:t) = if   a == (fst h) 
                       then True 
                       else nosPrimeiros a t 

--  j)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c) 
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = let (sx,sy,sz) = sumTriplos t 
                         in  (sx+x, sy+y, sz+z) 

-- 3

--  a) 
