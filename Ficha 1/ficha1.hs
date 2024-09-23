-- Ficha 1

perimetro :: Double -> Double
perimetro r = 2 * pi * r 

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

multiplo :: Int -> Int -> String  
multiplo m n = if mod n m == 0 
               then "True"
               else "False"  

multiplo' :: Int -> Int -> Bool 
multiplo' m n = mod n m == 0 

truncaImpar :: [a] -> [a]
truncaImpar l = if even (length l) 
                then l 
                else tail l   

max2 :: Int -> Int -> Int 
max2 a b = if a > b
           then a
           else b 

max3 :: Int -> Int -> Int -> Int 
max3 a b c = if (max2 a b) > c 
             then max2 a b 
             else c

max3' :: Int -> Int -> Int -> Int 
max3' a b c = if a > b 
              then max2 a c
              else max2 b c

nRaizes :: Int -> Int -> Int -> String
nRaizes a b c = if b^2 - 4 * c * a == 0 
                then "O polinomio tem uma raiz"
                else if b^2 - 4 * c * a > 0 
                     then "O polinomio tem duas raiz"
                     else "O polinomio nao tem raizes"                                                        

raizes :: Float -> Float -> Float -> [a]
raizes a b c = if nRaizes a b c == "O polinomio tem duas raiz"
               then [-b + sqrt (b^2 - 4 * c * a) / (2 * a),
                    -b - sqrt (b^2 - 4 * c * a) / (2 * a) ]
               else if nRaizes a b c == "O polinomio tem uma raiz" 
                    then [-b / (2 * a) ]
                    else []


