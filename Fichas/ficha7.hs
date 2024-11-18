-- // FICHA 7 \\ -- 

--   1. 

data ExpInt = Const Int
            | Simetric ExpInt
            | Mais     ExpInt ExpInt
            | Menos    ExpInt ExpInt
            | Mult     ExpInt ExpInt 

-- a) 
calcula :: ExpInt -> Int 
calcula (Const x)  
       = x 
calcula (Simetric x)  
       = - calcula x  
calcula (Mais x1 x2) 
       = calcula x1 + calcula x2 
calcula (Menos x1 x2) 
       = calcula x1 - calcula x2 
calcula (Mult x1 x2) 
       = calcula x1 * calcula x2 

-- b) 
infixa :: ExpInt -> String 
infixa (Const x) 
      = show x 
infixa (Simetric x) 
      = "( -" ++ infixa x ++ ")"
infixa (Mais x1 x2) 
      = "(" ++ infixa x1 ++ " + " ++ infixa x2 ++ ")" 
infixa (Menos x1 x2)
      = "(" ++ infixa x1 ++ " - " ++ infixa x2 ++ ")" 
infixa (Mult x1 x2)
      = "(" ++ infixa x1 ++ " * " ++ infixa x2 ++ ")"

-- c)
posfixa :: ExpInt -> String 
posfixa (Const x) 
       = show x 
posfixa (Simetric x)
       = "-" ++ posfixa x 
posfixa (Mais x1 x2) 
       = posfixa x1 ++ " " ++ posfixa x2 ++ " +" 
posfixa (Menos x1 x2) 
       = posfixa x1 ++ " " ++ posfixa x2 ++ " -" 
posfixa (Mult x1 x2) 
       = posfixa x1 ++ " " ++ posfixa x2 ++ " *" 


--   2. 

data RTree a = R a [RTree a]

-- a) 
soma :: Num a => RTree a -> a 
soma (R a [])
    = a 
soma (R a (h:t)) 
    = a + sum (map soma (h:t)) 

-- b) 
         
     
     