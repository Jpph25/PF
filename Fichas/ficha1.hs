--   // FICHA 1 \\

-- 1.

--   a)
-- Calculates the perimeter of a circunference  
perimeter :: Double -> Double 
perimeter r  
         = 2 * pi * r  

--   b)
-- Calculates the distance between two points 
dist :: (Double,Double) -> (Double,Double) -> (Double)
dist (x1,y1) (y2,x2) 
    = sqrt ((x1-x2)^2 + (y1-y1)^2)

--   c)
-- Creates a pair of the first and second element of a list 
fstUlt :: [a] -> (a,a)
fstUlt l 
      = (head l,last l)

--   d)
-- Verifies if the first number is a multiple of the second 
multiple :: Int -> Int -> Bool
multiple m n 
        = if   mod n m == 0 
          then True
          else False 

--   e)
-- When the number of elements is odd gives back the list without the fist element
-- When the number of elements is even gives back the same list
takeOdd :: [a] -> [a]
takeOdd l
       = if   odd (length l)
         then tail l   
         else l 

--   f)
-- Tells wich is bigger between two numbers 
max2 :: Int -> Int -> Int 
max2 a b 
    = if   a > b
      then a
      else b 

--   g)
-- Tells wich is bigger between three numbers 
max3 :: Int -> Int -> Int -> Int 
max3 a b c 
    = if   a > b 
      then max2 a c
      else max2 b c


-- 2.

--   a)
-- Tells how many roots a polynomial has 
nRoots :: Float -> Float -> Float -> String
nRoots a b c
      | b^2 - 4 * c * a == 0 = "The polynomial has one root" 
      | b^2 - 4 * c * a >  0 = "The polynomial has two roots"
      | otherwise            = "The polynomial has no root"

--   b)
-- Calculates the roots of the polynomials 
roots :: Float -> Float -> Float -> [Float]
roots a b c  
     |  nRoots a b c == "The polynomial has two roots"
      = [(-b + sqrt (b * b - 4 * c * a)) / (2 * a),
         (-b - sqrt (b * b - 4 * c * a)) / (2 * a) ]
     |  nRoots a b c == "The polynomial has one root"
      = [(-b) / (2 * a) ]
     | otherwise = [] 


-- 3.

type Hour = (Int,Int)

-- a)
-- Verifies if its a valid hour 
validHour :: Hour -> Bool
validHour (h,m) 
         =    h >= 0 && h < 24 
           && m >= 0 && m < 60 

--   b)
-- Verifies if an hour is after another one  
ordHour :: Hour -> Hour -> Bool
ordHour (h1,m1) (h2,m2) 
       | h1 > h2            = True 
       | h1 < h2 && m1 < m2 = True 
       | otherwise          = False

--   c) 
-- Convert hours into minutes 
convHtoM :: Hour -> Int 
convHtoM (h,m) 
        = h * 60 + m  

--   d) 
-- Convert minutes into hours 
convMtoH :: Int -> Hour
convMtoH m
        = (div m 60, m - 60 * div m 60)

--   e) 
-- Calculates the diference between two hours (in minutes) 
diffHoras :: Hour -> Hour -> Int
diffHoras (h1,m1) (h2,m2) 
         = abs (convHtoM (h1,m1) - convHtoM (h2,m2)) 

--   f)  
-- Adds minutes to an hour 
addMtoH :: Int -> Hour -> Hour
addMtoH m (h1,m1) 
       = convMtoH (m + convHtoM (h1,m1)) 


-- 4.

data Hour' = H Int Int 
           deriving (Show,Eq)

--   a)  
-- Verifies if its a valid hour
validHour' :: Hour' -> Bool
validHour' (H h m) 
          = if      h >= 0 && h < 24 
                 && m >= 0 && m < 60 
            then True
            else False

--   b)
-- Verifies if an hour is after another one 
horaOrd' :: Hour' -> Hour' -> Bool
horaOrd' (H h1 m1) (H h2 m2) 
        | h1 > h2            = True 
        | h1 < h2 && m1 < m2 = True 
        | otherwise          = False 

--   c)
--  Convert hours into minutes
convHtoM' :: Hour' -> Int 
convHtoM' (H h m) 
         = h * 60 + m 

--   d)
-- Convert minutes into hours 
convMtoH' :: Int -> Hour'  
convMtoH' m 
         = (H (div m 60) (m - 60 * div m 60)) 

--   e) 
-- Calculates the diference between two hours (in minutes) 
diffHoras' :: Hour' -> Hour' -> Int   
diffHoras' (H h1 m1) (H h2 m2) 
          = abs (convHtoM'(H h1 m1) - convHtoM'(H h2 m2))

--   f)
-- Adds minutes to an hour 
addMtoH' :: Int -> Hour' -> Hour' 
addMtoH' m (H h1 m1)
        = convMtoH' (m + convHtoM' (H h1 m1))  

-- 5.

data TrafficL =  Green
               | Yellow
               | Red
                deriving (Show,Eq)   

--   a)
-- Says the enxt colour of the Traffic light 
next :: TrafficL -> TrafficL
next l 
    | l == Green  = Yellow
    | l == Yellow = Red
    | otherwise   = Green

next' :: TrafficL -> TrafficL
next' Green  = Yellow
next' Yellow = Red
next' Red    = Green

--   b) 
-- Says if its obligatorie to stop (Red Light)  
stop :: TrafficL -> Bool
stop l  
    = l == Red 
      
stop' :: TrafficL -> Bool 
stop' Red = True 
stop' _   = False

--   c) 
-- Test if the Traffic light in a crossroad 
safe :: TrafficL -> TrafficL -> Bool
safe l1 l2  
    | (l1 == Red    && l2 == Green ) = True 
    | (l1 == Green  && l2 == Red   ) = True
    | (l1 == Yellow && l2 == Red   ) = True
    | (l1 == Red    && l2 == Yellow) = True 
    | otherwise                      = False

safe' :: TrafficL -> TrafficL -> Bool
safe'  Red  _   = True 
safe'  _    Red = True
safe'  _    _   = False
  