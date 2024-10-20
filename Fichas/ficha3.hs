-- // FICHA 3 \\ -- 

-- 1. 
data Hour = H Int Int
     deriving Show

type Stage = (Hour,Hour)

type Trip = [Stage] 

--   a) 
-- Tests if a stage is valid 
validstg :: Stage -> Bool 

validstg (H h1 m1, H h2 m2) 
        | h1 <  h2             = True 
        | h1 == h2 && m1 <  m2 = True 
        | otherwise 
         = False 
--   b) 
-- Tests if a trip is valid 
validtrp :: Trip -> Bool 
validtrp  []
        = error "Trip not constructed"
validtrp  [x]
        = validstg x 
validtrp ((H h11 m11, H h12 m12): (H h21 m21, H h22 m22): t)
        |(   validstgs  ((H h11 m11, H h12 m12): (H h21 m21, H h22 m22): t)
          && validstgss ((H h11 m11, H h12 m12): (H h21 m21, H h22 m22): t))
         == True    
         = True 
        | otherwise
         = False  
    where 
          validstgs [] 
                   = True 
          validstgs  (h:t) 
                    | validstg h && validstgs t = True
                    | otherwise
                     = False
        --             --
          validstgss []
                    = True 
          validstgss [x]
                    = True 
          validstgss [s1,s2]
                    = validstg (snd s1, fst s2)              
          validstgss (h:x:t)
                    | validstg (snd h, fst x) && validstgss t = True 
                    | otherwise
                     = False 

--   c) 
-- Calculates the hour that the trip started and ended 
hstrtend :: Trip -> Stage 
hstrtend []
        = error "Trip not constructed" 
hstrtend x  
        = (fst (head x), snd (last x))  

--   d) 
-- Calculates the duration of a trip 
durtrip :: Trip -> Int 
durtrip  [] 
       = error "Trip not constructed" 
durtrip (h:t) 
       |  validtrp (h:t) == True 
        = htom (snd h) - htom (fst h) + durtrip t   
       | otherwise
        = error "Invalid trip"
   where 
         htom (H h m) 
              = h * 60 + m  

--   e) 
-- Calculates the time that the trip was paused 
durpause :: Trip -> Int 
durpause  []
        = error "Trip not constructed" 
durpause  [x]
        = 0
durpause (h:x:t)
        | validtrp (h:x:t) == True 
         = htom (fst x) - htom (snd h) + durpause (x:t) 
        | otherwise
         = error "Invalid trip" 
    where 
          htom (H h m) 
               = h * 60 + m  

--   f) 
-- Calculates the total time of the trip 
durtotal :: Trip -> Int 
durtotal []
        = error "Trip not constructed"
durtotal [x]
        = htom (snd x) - htom (fst x) 
    where 
          htom (H h m) 
               = h * 60 + m         
durtotal (h:t)
        = htom (snd (last t)) - htom (fst h) 
    where
          htom (H h m) 
               = h * 60 + m   


-- 2.    

type Point = (Double, Double) 

type Polygon = [Point] 

data Figure = Circle    Point Double
            | Rectangle Point Point
            | Triangle  Point Point  Point 
             deriving (Show,Eq)

--   a) 
-- Calculates the size of the polygon 
splgon :: Polygon -> Double 
splgon []
      = error "Polygon not constructed"
splgon [x]
      = error "Polygon not constructed"
splgon [x,y] 
      = sqrt ((fst x - fst y)^2 + (snd x - snd y)^2)
splgon (h:x:t)
      = dist h x + splgon (x:t) 
  where 
        dist x y 
            = sqrt ((fst x - fst y)^2 + (snd x - snd y)^2) 

--   b) 
-- Tests if a polygon is closed 
plgonc :: Polygon -> Bool 
plgonc []
      = error "Polygon not constructed"
plgonc [x]
      = error "Polygon not constructed"
plgonc [x,y]
      = False 
plgonc (h:t) 
      | h == last t = True 
      | otherwise 
       = False 

--   c) 
--  Makes a list of triangles that sumed have the same area as the polygon
triangle :: Polygon -> [Figure]
triangle (h:x:y:t) 
        = Triangle h x y : triangle (x:y:t)
triangle  _
        = []

--   d) 
-- Calculates the area of a closed polygon 
areaplgn :: Polygon -> Double  
areaplgn (h:x:y:t)
        = 1/2 * abs (sumall (h:x:y:t)) 
   where 
         sumall  []
               = 0
         sumall (h:x:y:t) 
               = fst h * snd x - fst x * snd h + sumall (x:y:t)
         sumall  _ 
               = 0 
areaplgn  _ 
        = 0 

--   e)  
-- Make a given point the first element of a polygon 
fstpplgn :: Polygon -> Point -> Polygon
fstpplgn  [] (x,y)
        = []
fstpplgn (h:t) (x,y) 
        = (x,y) : dist t h (x,y)  
    where 
          dist :: Polygon -> Point -> Point -> Polygon 
          dist  [] _ _
              = [] 
          dist  (h:t) (x1,y1) (x2,y2) 
              = let npoint = (x2 - x1 + fst h, y2 - y1 + snd h) 
                in  npoint : dist t h npoint 
                  
 --   f) 
 -- Makes a polygon in between distance bigger 
zoom :: Double -> Polygon -> Polygon 
zoom _ [] 
    =  [] 
zoom  x (h:t) 
     = h : zom t x h 
 where 
       zom :: Polygon -> Double -> Point -> Polygon 
       zom  [] _ _ 
          = []
       zom (h:t) z (x,y) 
          = let newpoint = (z * (fst h - x), z * (snd h - y)) 
            in  newpoint : zom t z newpoint     


-- 3. 

data Contact = Home  Integer
             | Work  Integer
             | Phone Integer
             | Email String
             deriving Show

type Name = String

type Agenda = [(Name, [Contact])]

--   a) 
-- Adds a new name and contact to an agenda 
addEmail :: Name -> String -> Agenda -> Agenda 
addEmail n e a 
        = (n, [Email e]) : a  

--   b)
-- Serches for the email of a given name 
seeEmail :: Name -> Agenda -> Maybe [String] 
seeEmail _ []
        = Nothing 
seeEmail n (h:t)
        | n == fst h = Just (listEmail (snd h)) 
        | n /= fst h = seeEmail n t 
        | otherwise 
         = Nothing  
    where 
          listEmail :: [Contact] -> [String]
          listEmail  []
                   = []
          listEmail (Email e : t) 
                   = e : listEmail t  
          listEmail  (_ : t)
                   = listEmail t  

--   c) 
-- Gives a list of all telephone numbers in a list of contacts 
contPhone :: [Contact] -> [Integer]
contPhone  []
         = []
contPhone (Home c : t)
         = c : contPhone t 
contPhone (Work c : t)
         = c : contPhone t     
contPhone (Phone c : t)
         = c : contPhone t 
contPhone (_ : t) 
         = contPhone t 

--   d) 
-- Gives the home number from a given person
seeHome :: Name -> Agenda -> Maybe Integer   
seeHome _ []
        = Nothing 
seeHome n (h:t)
        | n == fst h = listHome (snd h)  
        | n /= fst h = seeHome n t 
        | otherwise 
         = Nothing  
    where 
          listHome :: [Contact] -> Maybe Integer  
          listHome  []
                  = Nothing
          listHome  (Home c : t) 
                  = Just c   
          listHome  (_ : t)
                  = listHome t  


-- 4. 

type Day   = Int

type Month = Int

type Year  = Int

type Name' = String

data Date  = D Day Month Year
           deriving Show

type BirthD = [(Name,Date)]

--   a)
-- Gives the birthday date of a given person of it is in the list 
search :: Name -> BirthD -> Maybe Date 
search _ []
      = Nothing 
search n (h:t)
      | n == fst h = Just (snd h)
      | otherwise 
       = search n t  

--   b) 
-- Calculates tha age of someone in a given date 
age :: Date -> Name -> BirthD -> Maybe Int 
age _ _ []
   = Nothing 
age (D d m y) n (h:t) 
   = cAge (search n (h:t)) (D d m y)  
 where 
       cAge :: Maybe Date -> Date -> Maybe Int 
       cAge Nothing _ 
           = Nothing 
       cAge (Just (D d1 m1 y1)) (D d2 m2 y2) 
           | m1 <= m2 && d1 <= d2 = Just (y2 - y1) 
           | y2 <  y1 = Nothing  
           | otherwise
            = Just (y2 - y1 - 1) 

--   c)
-- Tests wich date if a date is older than anther  
older :: Date -> Date -> Bool 
older (D d1 m1 y1) (D d2 m2 y2)
     | y1 <  y2 = True
     | y1 == y2 && m1 <  m2 = True 
     | y1 == y2 && m1 == m2 && d1 <  d2 = True 
     | otherwise 
      = False 

--   d) 
-- Oders a list of dates in acending order 
order :: BirthD -> BirthD 
order  []
     = []
order  [x]
     = [x] 
order (h:x:t) 
     | older (snd h) (snd x) = sndord h (order (x:t)) 
     | otherwise 
      = sndord x (order (h:t)) 
 where 
       sndord :: (Name, Date) -> BirthD -> BirthD 
       sndord x (h:t) 
             | older (snd x) (snd h) = x : (h:t) 
             | otherwise
              = h : sndord x t   

--   e) 
-- Makes a list with the name and age of people from a list of birthdays 
-- by acending order   
ordAge :: Date -> BirthD -> [(Name, Int)] 
ordAge _ []
      =  [] 
ordAge dt [x]
      = [(fst x , agec dt (snd x))] 
 where 
       agec :: Date -> Date -> Int
       agec (D d1 m1 y1) (D d2 m2 y2)
           | m2  < m1 || m2 <= m1 && d2 <= d1 = y1 - y2
           | y1  < y2 = 0 
           | otherwise 
            = y1 - y2 -1  
ordAge dt ((n, d):t)
      = ordr ((n, agec dt d) : ordAge dt t)     
 where 
       agec :: Date -> Date -> Int
       agec (D d1 m1 y1) (D d2 m2 y2)
           | m2  < m1 || m2 <= m1 && d2 <= d1 = y1 - y2
           | y1  < y2 = 0 
           | otherwise 
            = y1 - y2 -1 
       --               --       
       ordr :: [(Name, Int)] -> [(Name, Int)] 
       ordr  []
           = []
       ordr  [x]
           = [x]
       ordr ((n1,a1):(n2,a2):t) 
           | a1 >= a2 = (n2,a2) : ordr ((n1,a1):t) 
           | otherwise  
            = (n1,a1) : ordr ((n2,a2):t)  


-- 5.       

data Movement = Credit Float 
              | Debit  Float
              deriving Show

data Extract = Ext Float [(Date, String, Movement)]
             deriving Show

--   a) 
-- Makes a list of all movements above a certain value 
extValeu :: Extract -> Float -> [Movement] 
extValeu (Ext _ []) _  
        = []
extValeu (Ext x ((d, dc, Credit m) : t)) v  
        | v < m = Credit m : extValeu (Ext x t) v     
        | otherwise 
         = extValeu (Ext x t) v
extValeu (Ext x ((d, dc, Debit m) : t)) v  
        | v < m = Debit m : extValeu (Ext x t) v     
        | otherwise 
         = extValeu (Ext x t) v

--   b) 
-- Makes a list with the elements with a given description 
dcrptf :: Extract -> [String] -> [(Date, Movement)] 
dcrptf (Ext _ []) _  
        = []
dcrptf (Ext x ((d, dc, Credit m):t)) (dcg:tc)  
      | srchl (dcg:tc) dc =  (d, Credit m) : dcrptf (Ext x t) (dcg:tc)  
      | otherwise 
       = dcrptf (Ext x t) (dcg:tc) 
  where
       srchl :: [String] -> String -> Bool 
       srchl  [] _ 
            = False
       srchl (h:t) s 
            | h == s = True 
            | otherwise 
             = srchl t s 
dcrptf (Ext x ((d, dc, Debit m):t)) (dcg:tc)  
      | srchl (dcg:tc) dc =  (d, Debit m) : dcrptf (Ext x t) (dcg:tc)  
      | otherwise 
       = dcrptf (Ext x t) (dcg:tc) 
  where
       srchl :: [String] -> String -> Bool 
       srchl  [] _ 
            = False
       srchl (h:t) s 
            | h == s = True 
            | otherwise 
             = srchl t s 

--   c) 
-- Makes a pair with the total of credits and debits movements 
creDeb :: Extract -> (Float,Float) 
creDeb (Ext x ((d, dc, m):t))
      = (sumc ((d, dc, m):t), sumd ((d, dc, m):t)) 
 where 
       sumc :: [(Date, String, Movement)] -> Float 
       sumc  []
           = 0 
       sumc ((_, _, Credit m) : t)
           = m + sumc t 
       sumc ((_, _, _):t)
           = sumc t      
       --            -- 
       sumd :: [(Date, String, Movement)] -> Float 
       sumd  []
           = 0 
       sumd ((_, _, Debit m) : t)
           = m + sumd t 
       sumd ((_, _, _):t)
           = sumd t     

--   d) 
-- Calculate the total balance after the movementes 
balance :: Extract -> Float
balance (Ext x ((d, dc, m):t)) 
       = x - spent ((d, dc, m):t) 
  where 
        spent :: [(Date, String, Movement)] -> Float 
        spent  []
             = 0
        spent ((_, _, Credit m):t) 
             = m + spent t 
        spent ((_, _, Debit  m):t) 
             = m + spent t 
