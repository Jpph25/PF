-- // FICHA 8 \\ -- 

import Data.List 

--   1. 

data Frac = F Integer Integer 

-- a) 
-- Function that makes a fraction irreduceble 

irreduceble :: Frac -> Frac 
irreduceble (F n d) 
           = F (s * n') d'     
      where 
            bcd :: Integer -> Integer -> Integer  
            bcd x y 
               | x == 0 || y == 0 = 1  
               | x == y = x 
               | x  > y = bcd (x - y) y 
               | x <  y = bcd  x (y - x) 
            -- 
            n' = div (abs n) m 
            d' = div (abs d) m 
            m  = bcd (abs n) (abs d) 
            s  = if    n * d < 0 
                  then -1 
                  else  1    

-- b) 
-- Comprassion of fractions 

instance Eq Frac where 
 (F n1 d1) == (F n2 d2) 
   = n1 * d2 == n2 * d1 

-- c) 
-- Order of fractions 

instance Ord Frac where 
 (F n1 d1) <= (F n2 d2) 
  = n1 * d2 <= n2 * d1 

-- d) 
-- Representacion of fractions

instance Show Frac where 
 show (F n 1) = "(" ++ show n ++ ")"     
 show (F n d) = "(" ++ show n ++ "/" ++ show d ++ ")" 

 -- e) 
 -- accounts with fractions 

instance Num Frac where 
 (F n1 d1) + (F n2 d2) 
        = F n d 
  where 
        n = (n1 * d2) + (n2 * d1) 
        d =  d1 * d2
 (F n1 d1) - (F n2 d2) 
        = F n d
  where
        n = (n1 * d2) - (n2 * d1) 
        d = d1 * d2 
 (F n1 d1) * (F n2 d2) 
        = F (n1 * n2) (d1 * d2) 
 abs (F n d) 
        = F (abs n) (abs d) 
 signum (F n d) 
       | n     == 0 = F   0  1 
       | n * d  < 0 = F (-1) 1 
       | n * d >  0 = F   1  1 
 fromInteger n = F n 1  

-- f) 
-- Takes from a list all elemente that are double of a given fraction

filtDbl :: Frac -> [Frac] -> [Frac] 
filtDbl _ [] 
       =  [] 
filtDbl (F n d) ((F n' d'):t) 
       = filter (\ (F n d) -> F n d >= 2 * F n' d') (F n' d':t)    


--   2. 

data Exp a = Const a
           | Simetric (Exp a)
           | Plus     (Exp a) (Exp a)
           | Minus    (Exp a) (Exp a)
           | Mult     (Exp a) (Exp a) 

-- a) 
-- Representation of the expressions 

instance Show a => Show (Exp a) where
 show (Const    x) = show x 
 show (Simetric x) = "-" ++ show x 
 show (Plus   x y) = show x ++ " + " ++ show y 
 show (Minus  x y) = show x ++ " - " ++ show y 
 show (Mult   x y) = show y ++ " * " ++ show y 

-- b) 
-- Equivalence of expressions 

calc :: (Show a, Eq a, Num a) => Exp a -> a  
calc (Const x) 
    = x 
calc (Simetric x) 
    = - (calc x) 
calc (Plus x y) 
    = calc x + calc y        
calc (Minus x y) 
    = calc x - calc y
calc (Mult x y) 
    = calc x * calc y

instance (Show a, Eq a, Num a) => Eq (Exp a) where 
  x == y = calc x == calc y  

-- c) 
-- Accounts of expressions  

instance (Ord a, Show a, Eq a, Num a) => Num (Exp a) where 
   x + y = Const (calc x + calc y) 
   x - y = Const (calc x - calc y) 
   x * y = Const (calc x * calc y) 
   abs x = Const (abs (calc x)) 
   signum x 
         | calc x  > 0 = Const 1 
         | calc x <  0 = Simetric 1 
         | calc x == 0 = Const 0 
   fromInteger x = Const (fromInteger x)      


--   3. 

data Movement = Credito Float 
               | Debito  Float

data Date = D Int Int Int

data Extract = Ext Float [(Date, String, Movement)] 

-- a) 
-- Representation of a date 

instance Show Date where 
  show (D d m y) = show d ++ "/" ++ show m ++ "/" ++ show y   

-- b)
-- Comparassion of a date 

instance Eq Date where 
 (D d1 m1 y1) == (D d2 m2 y2) 
  = d1 == d2 && m1 == m2 && y1 == y2  

instance Ord Date where 
 (D d1 m1 y1) <= (D d2 m2 y2)
  = d1 < d2                       || 
    d1 > d2 && m1 < m2            || 
    d1 > d2 && m1 > m2 && y1 < y2 

-- c) 
-- Orders an extract by date 

ordExt :: Extract -> Extract 
ordExt (Ext x l)  
      = Ext x (sortOn (\ (d, _, _) -> d) l)   


-- d) 
-- Representation of an extarct 

{-
instance Show Extracto where 
         show (Ext si lMov)
             = saldoA si ++ cabeçalho ++ listaMov lMov ++ saldoFinal si lMov

saldoA :: Float -> String 
saldoA x 
      = "Saldo Anterior: " ++ show x ++ "\n" 

preenche :: String -> Int -> String 
preenche s n 
        = replicate (n - length s) ' ' 

cabeçalho =  replicate 90 '_' ++ "\n" 
          ++ "Data"      ++ preenche "Data"      20 
          ++ "Descricao" ++ preenche "Descricao" 30
          ++ "Credito"   ++ preenche "Credito"   20 
          ++ "Debito"    ++ preenche "Debito"    20
          ++ "\n" 

listaMov :: [(Data, String, Movimento)] -> String 
listaMov [()]  
-}