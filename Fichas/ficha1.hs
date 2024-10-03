-- Ficha 1

--  1

-- a)
-- Calculo do perimetro de uma circunferênci
perimetro :: Double -> Double
perimetro r = 2 * pi * r 

-- b)
-- Calculo da distância entre dois pontos
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- c)
-- Lista do primeiro e ultimo elemento de uma lista
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

-- d)
-- Verifica se um numero é multiplo de outro
multiplo :: Int -> Int -> String  
multiplo m n = if   mod n m == 0 
               then "True"
               else "False"  


multiplo' :: Int -> Int -> Bool 
multiplo' m n = mod n m == 0 

-- e)
-- Devolve a lista caso a quantidade de elemnetos seja par 
-- e devolve tudo menos o primeiro elemento caso seja impar
truncaImpar :: [a] -> [a]
truncaImpar l = if   even (length l) 
                then l 
                else tail l   

-- f)
-- Diz qual é o maior numero entre dois numeros
max2 :: Int -> Int -> Int 
max2 a b = if   a > b
           then a
           else b 

-- g)
-- Diz qual é o maior numero entre três numeros
max3 :: Int -> Int -> Int -> Int 
max3 a b c = if  (max2 a b) > c 
             then max2 a b 
             else c

max3' :: Int -> Int -> Int -> Int 
max3' a b c = if   a > b 
              then max2 a c
              else max2 b c

--  2

-- a)
-- Diz quantas raizes um polinomio de grau 2 têm 
nRaizes :: Float -> Float -> Float -> String
nRaizes a b c = if   b^2 - 4 * c * a == 0 
                then "O polinomio tem uma raiz"
                else if   b^2 - 4 * c * a > 0 
                     then "O polinomio tem duas raizes"
                     else "O polinomio nao tem raizes"                                                        

-- b)
-- Calcula as raizes de um polinomio 
raizes :: Float -> Float -> Float -> [Float]
raizes a b c = if   nRaizes a b c == "O polinomio tem duas raizes"
               then [(-b + sqrt (b * b - 4 * c * a)) / (2 * a),
                     (-b - sqrt (b * b - 4 * c * a)) / (2 * a) ]
               else if   nRaizes a b c == "O polinomio tem uma raiz" 
                    then [(-b) / (2 * a) ]
                    else [] 

--  3

type Hora = (Int,Int)

-- a)
-- Verifica se uma hora é valida
horaValid :: Hora -> Bool
horaValid (h,m) =    h >= 0 && h < 24 
                  && m >= 0 && m < 60 
               
-- b)
-- Diz se a primeira hora é depois da segunda hora
horaOrd :: Hora -> Hora -> Bool
horaOrd (h1,m1) (h2,m2) = if   (   horaValid (h1,m1) 
                                && horaValid (h2,m2)) == True
                          then if   h1 > h2 
                               then True
                               else if   h1 == h2 
                                    then m1 > m2 
                                    else False     
                          else False      

-- c) 
-- Converte horas em minutos 
convHtoM :: Hora -> Int 
convHtoM (h,m) = h * 60 + m  

-- d)
-- Converte minutos em horas
convMtoH :: Int -> Hora
convMtoH m = (div m 60, m - 60 * div m 60)

-- e) 
-- Diferença entre duas horas (em minutos)
diffHoras :: Hora -> Hora -> Int
diffHoras (h1,m1) (h2,m2) = abs(convHtoM (h1,m1) - convHtoM(h2,m2)) 

-- f) 
-- Adiciona minutos a um par de horas
addMtoH :: Int -> Hora -> Hora
addMtoH m (h1,m1) = convMtoH(m + convHtoM(h1,m1)) 

--  4 

data Hora' = H Int Int 
           deriving (Show,Eq)

-- a) 
-- Verifica se uma hora é valida
horaValid' :: Hora' -> Bool
horaValid' (H h m) = if    h >= 0 && h < 24 
                        && m >= 0 && m < 60 
                     then True
                     else False

-- b)
-- Testa se a primeira hora é depois da segunda hora  
horaOrd' :: Hora' -> Hora' -> Bool
horaOrd' (H h1 m1) (H h2 m2) 
                            | h1 > h2 = True 
                            | h1 < h2 && m1 < m2 = True 
                            | otherwise = False 

-- c) 
-- Coverte horas em minutos 
convHtoM' :: Hora' -> Int 
convHtoM' (H h m) = h * 60 + m 
  
-- d)
-- Converte minutos em horas
convMtoH' :: Int -> Hora'  
convMtoH' m = (H (div m 60) (m - 60 * div m 60))                                  

-- e) 
-- Calcula a diferença entre duas horas 
diffHoras' :: Hora' -> Hora' -> Int   
diffHoras' (H h1 m1) (H h2 m2) = abs(convHtoM'(H h1 m1) - convHtoM'(H h2 m2))   

-- f) 
-- Adiciona minutos a uma dada hora 
addMtoH' :: Int -> Hora' -> Hora' 
addMtoH' m (H h1 m1) = convMtoH'(m + convHtoM'(H h1 m1))  

--  5

data Semaforo =  Verde
              | Amarelo
              | Vermelho
              deriving (Show,Eq)  

-- a)
-- Calcula a proxima cor do semafro 
next :: Semaforo -> Semaforo
next s 
      | s == Verde   = Amarelo
      | s == Amarelo = Vermelho
      | otherwise = Verde 

next' :: Semaforo -> Semaforo
next' Verde    = Amarelo 
next' Amarelo  = Vermelho
next' Vermelho = Verde

-- b) 
-- Indica se é obrigatorio parar
stop :: Semaforo -> Bool
stop s = if   s == Vermelho 
         then True
         else False

stop' :: Semaforo -> Bool 
stop' Vermelho = True 
stop' _        = False   
