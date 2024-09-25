-- Ficha 1

-- Calculo do perimetro de uma circunferênci
perimetro :: Double -> Double
perimetro r = 2 * pi * r 

-- Calculo da distância entre dois pontos
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Lista do primeiro e ultimo elemento de uma lista
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

-- Verifica se um numero é multiplo de outro
multiplo :: Int -> Int -> String  
multiplo m n = if mod n m == 0 
               then "True"
               else "False"  

multiplo' :: Int -> Int -> Bool 
multiplo' m n = mod n m == 0 

-- Devolve a lista caso a quantidade de elemnetos seja par e devolve tudo menos o primeiro elemento caso seja impar
truncaImpar :: [a] -> [a]
truncaImpar l = if even (length l) 
                then l 
                else tail l   

-- Diz qual é o maior numero entre dois numeros
max2 :: Int -> Int -> Int 
max2 a b = if a > b
           then a
           else b 

-- Diz qual é o maior numero entre três numeros
max3 :: Int -> Int -> Int -> Int 
max3 a b c = if (max2 a b) > c 
             then max2 a b 
             else c

max3' :: Int -> Int -> Int -> Int 
max3' a b c = if a > b 
              then max2 a c
              else max2 b c

-- Diz quantas raizes um polinomio de grau 2 têm 
nRaizes :: Float -> Float -> Float -> String
nRaizes a b c = if b^2 - 4 * c * a == 0 
                then "O polinomio tem uma raiz"
                else if b^2 - 4 * c * a > 0 
                     then "O polinomio tem duas raizes"
                     else "O polinomio nao tem raizes"                                                        


-- Calcula as raizes de um polinomio 
raizes :: Float -> Float -> Float -> [Float]
raizes a b c = if nRaizes a b c == "O polinomio tem duas raizes"
               then [(-b + sqrt (b * b - 4 * c * a)) / (2 * a),
                    (-b - sqrt (b * b - 4 * c * a)) / (2 * a) ]
               else if nRaizes a b c == "O polinomio tem uma raiz" 
                    then [(-b) / (2 * a) ]
                    else [] 



