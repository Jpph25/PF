-- Ficha 1

--  1

-- a)
-- Perimeter if a circunference
perimeter :: Double -> Double
perimeter r = 2 * pi * r 

-- b)
-- Distance of two points 
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- c)
-- Creates pair of the first and last element of a list
fstUlt :: [a] -> (a,a)
fstUlt l = (head l,last l)

-- d)
-- Verifies if the first number is a multiple of the second  
multiple :: Int -> Int -> Bool  
multiple m n = if   mod n m == 0 
               then True
               else False 

-- e)
-- If the legth of the list is even gives the list 
-- if not takes the first element of the list 
truncaOdd :: [a] -> [a]
truncaOdd l = if   even (length l) 
                then l 
                else tail l   

-- f)
-- Wich nun«mber is bigger between two numbers
max2 :: Int -> Int -> Int 
max2 a b = if   a > b
           then a
           else b 

-- g)
-- Wich nun«mber is bigger between three numbers
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
-- How many roots a equation has 
nRoots :: Float -> Float -> Float -> String
nRoots a b c = if   b^2 - 4 * c * a == 0 
                then "The polynomial has one root"
                else if   b^2 - 4 * c * a > 0 
                     then "The polynomial has two roots"
                     else "The polynomial has no root"                                                        

-- b)
-- Calculates the roots of the polynomials  
roots :: Float -> Float -> Float -> [Float]
roots a b c = if   nRoots a b c == "The polynomial has two roots"
               then [(-b + sqrt (b * b - 4 * c * a)) / (2 * a),
                     (-b - sqrt (b * b - 4 * c * a)) / (2 * a) ]
               else if   nRoots a b c == "The polynomial has one root"  
                    then [(-b) / (2 * a) ]
                    else [] 

--  3

type Hora = (Int,Int)

-- a)
-- Verifies if its a valid hour 
validHour :: Hora -> Bool
validHour (h,m) =    h >= 0 && h < 24 
                  && m >= 0 && m < 60 
               
-- b)
-- Says if an hour is after another
ordHour :: Hora -> Hora -> Bool
ordHour (h1,m1) (h2,m2) 
                        | h1 > h2 = True 
                        | h1 < h2 && m1 < m2 = True 
                        | otherwise = False

-- c) 
-- Convert hours into minutes
convHtoM :: Hora -> Int 
convHtoM (h,m) = h * 60 + m  

-- d)
-- Convert minutes into hours 
convMtoH :: Int -> Hora
convMtoH m = (div m 60, m - 60 * div m 60)

-- e) 
-- Diference between two hours (in minutes) 
diffHoras :: Hora -> Hora -> Int
diffHoras (h1,m1) (h2,m2) = abs(convHtoM (h1,m1) - convHtoM(h2,m2)) 

-- f) 
-- Adds minutes to an hour 
addMtoH :: Int -> Hora -> Hora
addMtoH m (h1,m1) = convMtoH(m + convHtoM(h1,m1)) 

--  4 

data Hora' = H Int Int 
           deriving (Show,Eq)

-- a) 
-- Verifies if its a valid hour 
validHour' :: Hora' -> Bool
validHour' (H h m) = if    h >= 0 && h < 24 
                        && m >= 0 && m < 60 
                     then True
                     else False

-- b)
-- Says if an hour is after another 
horaOrd' :: Hora' -> Hora' -> Bool
horaOrd' (H h1 m1) (H h2 m2) 
                            | h1 > h2 = True 
                            | h1 < h2 && m1 < m2 = True 
                            | otherwise = False 

-- c) 
-- Convert hours into minutes 
convHtoM' :: Hora' -> Int 
convHtoM' (H h m) = h * 60 + m 
  
-- d)
-- Convert minutes into hours 
convMtoH' :: Int -> Hora'  
convMtoH' m = (H (div m 60) (m - 60 * div m 60))                                  

-- e) 
-- Diference between two hours (in minutes) 
diffHoras' :: Hora' -> Hora' -> Int   
diffHoras' (H h1 m1) (H h2 m2) = abs(convHtoM'(H h1 m1) - convHtoM'(H h2 m2))   

-- f) 
-- Adds minutes to an hour 
addMtoH' :: Int -> Hora' -> Hora' 
addMtoH' m (H h1 m1) = convMtoH'(m + convHtoM'(H h1 m1))  

--  5

data Semaforo =  Verde
              | Amarelo
              | Vermelho
              deriving (Show,Eq)  

-- a)
-- Says the enxt colour of the Traffic light 
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
-- Says if its obligatorie to stop  
stop :: Semaforo -> Bool
stop s = if   s == Vermelho 
         then True
         else False

stop' :: Semaforo -> Bool 
stop' Vermelho = True 
stop' _        = False   

-- c) 
-- Test if the Traffic light in a crossroad 
safe :: Semaforo -> Semaforo -> Bool
safe 