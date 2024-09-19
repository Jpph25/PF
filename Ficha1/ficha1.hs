-- Exercicios Ficha 1

perimetro :: Double -> Double  
perimetro r = 2 * pi * r 

dist :: (Double,Double) -> (Double,Double) -> Double 
dist (x,y) (z,w) = sqrt ((x-z)^2 + (y-w)^2) 

primUlt :: [a] -> (a,a) 
primUlt l = (head l, last l) 

multiplo :: Int -> Int -> String
multiplo m n = if mod m n == 0 then "Sim" 
                               else "Nao" 

multiplo' :: Int -> Int -> Bool 
multiplo' m n = mod m n == 0 

truncaImpar :: [a] -> [a]
truncaImpar l = if even (length l) then l 
                                   else tail l

truncaImpar' :: [a] -> [a] 
truncaImpar' l = if mod (length l) 2 == 0 then l
                                          else tail l 

--max2 :: a -> a -> a
--max2 x y = if x > y then x 
--                    else y 

--max3 :: a -> a -> a ->a
--max3 a b c = max2 (max2 c b) a 
-- dois if's tentar


                                             