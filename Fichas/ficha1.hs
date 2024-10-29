--   // FICHA 1 \\

import Data.Char (ord, chr) 

-- 1.

--   a)
-- Calculates the perimeter of a circunference  
perimeter' :: Double -> Double 
perimeter' r  
         = 2 * pi * r  

--   b)
-- Calculates the distance between two points 
dist :: (Double,Double) -> (Double,Double) -> Double
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
-- Convertes hours into minutes 
convHtoM :: Hour -> Int 
convHtoM (h,m) 
        = h * 60 + m  

--   d) 
-- Convertes minutes into hours 
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
-- Convertes hours into minutes
convHtoM' :: Hour' -> Int 
convHtoM' (H h m) 
         = h * 60 + m 

--   d)
-- Convertes minutes into hours 
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


-- 6.

data Point = Cartesian  Double Double 
           | Polar      Double Double
           deriving (Show,Eq)

--   a)
-- Calculates the distance of a point to the vertical axis
posX :: Point -> Double
posX (Cartesian x y) 
    = x
posX (Polar d a)
    = d * cos a       

--   b)
-- Calculates the distance of a point to the horizontal axis
posY :: Point -> Double
posY (Cartesian x y)
    = y
posY (Polar d a)
    = d * sin a 

--   c)
-- Calculates the distance of a point to the origin 
radius :: Point -> Double
radius (Cartesian x y)
      = sqrt (x^2 + y^2) 
radius (Polar d a)
      = d 

--   d)
-- Calculates the angle of the vector with the horizontal axis (Degrees)  
angle :: Point -> Double 
angle (Cartesian x y)
     = (atan2 x y) * 180/pi    
angle (Polar d a)
     = a 

--   e)
-- Calculates the distance between two points 
dist' :: Point -> Point -> Double 
dist' (Cartesian x1 y1) (Cartesian x2 y2)
     = sqrt ((x1 - x2)^2 + (y1 - y2)^2) 
dist' (Polar d1 a1) (Polar d2 a2) 
     = let x1 = posX (Polar d1 a1)
           x2 = posX (Polar d2 a2)
           y1 = posY (Polar d1 a1)
           y2 = posY (Polar d2 a2)  
       in  sqrt ((x1 - x2)^2 + (y1 - y2)^2)


-- 7.

type Point' = (Double,Double) 

data Figure = Circle   Point' Double
            | Rectangle Point' Point'
            | Triangle  Point' Point' Point' 
             deriving (Show,Eq)

--   a)
-- Tests if the figure is a poligon 
poligon :: Figure -> Bool 
poligon (Circle (x,y) r) 
       = False  
poligon (Rectangle (x1,y1) (x2,y2)) 
       | y1 == y2 || x1 == x2 = False
       | otherwise = True 
poligon (Triangle (x1,y1) (x2,y2) (x3,y3)) 
       | x1 == x2 || x1 == x3 || x2 == x3 = False
       | y1 == y2 || y1 == y3 || y2 == y3 = False      
       | otherwise = True

--   b) 
-- Makes the list of veticies of the poligon 
vertecies :: Figure -> [Point'] 
vertecies (Circle (x,y) r)
         = []
vertecies (Rectangle (x1,y1) (x2,y2))
         = [(x1,y1),(x1,y2),(x2,y1),(x2,y2)] 
vertecies (Triangle (x1,y1) (x2,y2) (x3,y3))
         = [(x1,y1),(x2,y2),(x3,y3)] 

--   c)
-- Calculate the area of a Figure 
area :: Figure -> Double 
area (Circle (x,y) r)
    = pi * r^2
area (Rectangle (x1,y1) (x2,y2))
    = let lx = abs (x1 - x2) 
          ly = abs (y1 - y2)
      in  lx * ly 
area (Triangle (x1,y1) (x2,y2) (x3,y3)) 
    = let base  = sqrt ((x1 - x2)^2 + (y1 - y2)^2)    
          pmx   = (x1 + x2)/2
          pmy   = (y1 + y2)/2
          heigh = sqrt ((pmx - x3)^2 + (pmy - y3)^2) 
      in  (base * heigh)/2     

--   d) 
-- Calculates the perimeter of the figures
perimeter :: Figure -> Double 
perimeter (Circle (x,y) r)
         = 2 * pi * r
perimeter (Rectangle (x1,y1) (x2,y2))
         = let lx = abs (x1 - x2)
               ly = abs (y1 - y2)
           in  2 * lx + 2 * ly 
perimeter (Triangle (x1,y1) (x2,y2) (x3,y3))
         = let l1 = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
               l2 = sqrt ((x1 - x3)^2 + (y1 - y3)^2)
               l3 = sqrt ((x2 - x3)^2 + (y2 - y3)^2) 
           in  l1 + l2 + l3    


-- 8. 

--   a)
-- Verifies if a given character is in lowercas
isLower :: Char -> Bool
isLower c 
       = ord c >= ord 'a' && ord c <= ord 'z'   

--   b) 
-- Verifies if a given character is a digit 
isDigit :: Char -> Bool 
isDigit c
       = ord c >= ord '0' && ord c <= ord '9' 

--   c) 
-- Verifies if a given character is a letter 
isAlpha :: Char -> Bool 
isAlpha c
       =    ord c >= ord 'A' && ord c <= ord 'Z' 
         || ord c >= ord 'a' && ord c <= ord 'z' 

--   d)
-- Convertes a given letter into is capital version
toUpper :: Char -> Char 
toUpper c
       |  ord c >= ord 'A' && ord c <= ord 'Z' 
        = c 
       |  ord c >= ord 'a' && ord c <= ord 'z'
        = chr (ord 'A' + (ord c - ord 'a')) 
       |  otherwise = error "Invalid character"   

--   f)
-- Covertes a digit into is respective number 
digitToInt :: Char -> Int
digitToInt c
          |  ord c >= ord '0' && ord c <= ord '9'
           = ord c  - ord '0' 
          |  otherwise = error "Invalid character"  