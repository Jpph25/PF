-- // FICHA 6 \\ -- 

--   1. 

data BTree a = Empty
             | Node a (BTree a) (BTree a)
            deriving Show 

-- Exemplos 

tree1 = Node 2 
       (Node 3 Empty Empty) 
       (Node 1 (Node 4 Empty Empty) Empty) 

tree2 = Node 2
       (Node 3 Empty Empty) 
       (Node 4 (Node 3 (Node 1 Empty Empty) Empty) Empty)

tree3 = Node (1,2,3)
       (Node (4,5,6) Empty Empty)
       (Node (7,8,9) (Node (3,2,1) Empty Empty) Empty) 

-- a) 
-- Calculates the heigth of the tree 

heigth :: BTree a -> Int 
heigth Empty 
      = 0 
heigth (Node x t1 t2) 
      = 1 + max (heigth t1) (heigth t2)  

-- b) 
-- Calculates the number of nodes of a tree 

countNodes :: BTree a -> Int 
countNodes Empty 
          = 0 
countNodes (Node x t1 t2) 
          = 1 + heigth t1 + heigth t2 

-- c) 
-- Calculates the number of leaves of a tree 

leaves :: BTree a -> Int 
leaves Empty 
      = 0 
leaves (Node x Empty Empty) 
      = 1  
leaves (Node x t1 t2) 
      = leaves t1 + leaves t2 

-- d) 
-- Cuts all elements oof a tree when in a determined heigth 

cut :: Int -> BTree a -> BTree a 
cut _ Empty 
   =  Empty
cut 0 _ 
   = Empty  
cut c (Node x t1 t2)  
   = Node x (cut (c - 1) t1) (cut (c - 1) t2) 

-- e) 
-- Follows a path throw the tree 
 
path :: [Bool] -> BTree a -> [a] 
path  [] _  
    = []
path _ Empty 
    = []
path (h:t) (Node x t1 t2) 
    | h = x : path t t2 
    | otherwise 
     = x : path t t1 

-- f) 
-- Gives the simetric tree 

mirror :: BTree a -> BTree a 
mirror  Empty 
      = Empty
mirror (Node x t1 t2) 
      = Node x (mirror t2) (mirror t1)

-- g) 
-- Puts two trees together using a given function 

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ 
         = Empty 
zipWithBT _ _ Empty 
         = Empty 
zipWithBT f (Node x1 t1 t2) (Node x2 t1' t2') 
         = Node (f x1 x2) (zipWithBT f t1 t1') (zipWithBT f t2 t2') 

-- h) 
-- Make a Tree of triples into three trees  

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c) 
unzipBT Empty 
       = (Empty, Empty, Empty)
unzipBT  (Node (x,y,z) t1 t2) 
       = (unzipx (Node (x,y,z) t1 t2), 
          unzipy (Node (x,y,z) t1 t2), 
          unzipz (Node (x,y,z) t1 t2) )  
  where
       unzipx  Empty 
             = Empty 
       unzipx (Node (x,y,z) t1 t2) 
             = Node x (unzipx t1) (unzipx t2)
      -- 
       unzipy  Empty 
             = Empty 
       unzipy (Node (x,y,z) t1 t2) 
             = Node y (unzipy t1) (unzipy t2)
      -- 
       unzipz  Empty 
             = Empty 
       unzipz (Node (x,y,z) t1 t2) 
             = Node z (unzipz t1) (unzipz t2)         

--   2.

-- Exemplo 

treeS =  Node 10 
        (Node 5  (Node 3 Empty (Node 4 Empty Empty)) Empty)
        (Node 15  Empty (Node 20  Empty Empty)) 

-- a)
-- Searches the smallest element of a tree  

minBT :: Ord a => BTree a -> a 
minBT  Empty
     = error "Invalid tree" 
minBT (Node x Empty _) 
     = x 
minBT (Node x t1 t2) 
     = minBT t1  

-- b)
-- Removes the smallest element of the tree 

noMinBT :: Ord a => BTree a -> BTree a 
noMinBT  Empty 
       = error "Invalid tree"
noMinBT (Node x Empty t2)
       = t2 
noMinBT (Node x t1 t2) 
       = Node x (noMinBT t1) t2   

-- c) 
-- Calculate both previous functions

minNmin :: Ord a => BTree a -> (a, BTree a)
minNmin  Empty
       = error "Invalid tree" 
minNmin (Node x Empty t2) 
       = (x, t2) 
minNmin (Node x t1 t2) 
       = (min, Node x nMin t2) 
  where 
       (min, nMin) = minNmin t1 

-- d) 
-- Removes a given element of a tree 

remove :: Ord a => a -> BTree a -> BTree a 
remove _ Empty 
      = error "Invalid tree"
remove y (Node x t1 t2) 
      | y == x 
       = remRoot (Node x t1 t2) 
      | y <= x 
       = Node x (remove y t1) t2 
      | otherwise 
       = Node x  t1 (remove y t2)  
 where   
       glue tree (Node x Empty t2) 
           = Node x tree t2  
       glue tree (Node x (Node y t1' t2') t2)
           = Node x (Node y (glue tree t1') t2') t2  
       -- 
       remRoot (Node x Empty Empty) 
              = Empty 
       remRoot (Node x Empty t2) 
              = t2 
       remRoot (Node x t1 Empty) 
              = t1 
       remRoot (Node x t1 t2) 
              = glue t1 t2   


--   3. 

type Student = (Number, Name, Regime, Grade)

type Number = Int

type Name = String

data Regime = ORD -- Ordinary 
            | WS  -- Working student
            | GU  -- Grade upgrade 
            deriving Show

data Grade = Aprov Int
           | Rep
           | Missed 
           deriving Show

-- Binary tree of schearch (ordered by number)
type Class = BTree Student  

-- Exemplo 

classEx :: Class
classEx = Node (100, "Joao", ORD, Aprov 18) 
         (Node (90, "Ines", WS, Aprov 20) 
               (Node (85, "Leticia", GU, Missed) Empty Empty) Empty)  
         (Node (105, "Matilde", ORD, Rep) Empty Empty) 

-- a) 
-- Verifies if a student with a determined number is registered 

regsNum :: Number -> Class -> Bool 
regsNum x Empty 
       =  False 
regsNum x (Node (nb, na, r, g) t1 t2) 
       | x == nb = True 
       | x  > nb = regsNum x t2 
       | otherwise 
        = regsNum x t1  

-- b) 
-- Verifies if a student with a determined name is registered 

regsNam :: Name -> Class -> Bool 
regsNam x Empty 
       =  False 
regsNam x (Node (nb, na, r, g) t1 t2) 
       | x == na = True 
       | regsNam x t1 || regsNam x t2 
        = True    
       | otherwise 
        = False 

-- c) 
-- Makes the list of working studentes (Ordered by number) 

workStd :: Class -> [(Number, Name)] 
workStd Empty 
       = [] 
workStd (Node (nb, na, WS, g) t1 t2) 
       = workStd t1 ++ [(nb, na)] ++ workStd t2
workStd (Node (nb, na, r, g) t1 t2)
       = workStd t1 ++ workStd t2

-- d) 
-- Calculates the grade of a student 

grade :: Number -> Class -> Maybe Grade 
grade x (Node (nb, na, r, g) t1 t2) 
     | regsNum x (Node (nb, na, r, g) t1 t2) == False 
      = Nothing 
     | x == nb 
      = Just g 
     | x  > nb 
      = grade x t2 
     | otherwise 
      = grade x t1

-- e) 
-- Calculates the percentage of studentes that missed 

percMisse :: Class -> Float 
percMisse clas  
         = (100 * misses clas) 
          / total clas  
    where 
         misses  Empty 
               = 0.0 
         misses (Node (nb, na, r, Missed) t1 t2) 
               = 1.0 + misses t1 + misses t2 
         misses (Node (nd, na, r, g) t1 t2) 
               = misses t1 + misses t2 
         -- 
         total Empty 
              = 0.0 
         total (Node (nd, na, r, g) t1 t2)
              = 1.0 + total t1 + total t2  

-- f) 
-- Calculates the averege grade of the aproved students 

avrgAprov :: Class -> Float 
avrgAprov clas 
         = sumAprov clas / totalAprov clas
    where 
         sumAprov  Empty 
                 = 0.0  
         sumAprov (Node (nb, na, r, Aprov g) t1 t2) 
                 = fromIntegral g + sumAprov t1 + sumAprov t2 
         sumAprov (Node (nb, na, r, g) t1 t2)
                 = sumAprov t1 + sumAprov t2 
         -- 
         totalAprov Empty 
                   = 0.0 
         totalAprov (Node (nb, na, r, Aprov g) t1 t2)
                   = 1.0 + totalAprov t1 + totalAprov t2 
         totalAprov (Node (nb, na, r, g) t1 t2) 
                   = totalAprov t1 + totalAprov t2  

-- g) 
-- Caculate the racio of studentes that are aproved 

aprovR :: Class -> Float 
aprovR Empty 
      = 0.0 
aprovR clas 
      = let (aprvd, total) = aTotalt clas 
        in if   aprvd == 0.0 
           then 0.0 
           else aprvd / total 

aTotalt :: Class -> (Float, Float) 
aTotalt Empty 
       = (0, 0) 
aTotalt (Node (nb, na, r, Aprov g) t1 t2)      
       = let (aprvdL, totalL) = aTotalt t1 
             (aprvdR, totalR) = aTotalt t2 
         in  (1 + aprvdL + aprvdR, 1 + totalL + totalR) 
aTotalt (Node (nb, na, r, g) t1 t2)      
       = let (aprvdL, totalL) = aTotalt t1 
             (aprvdR, totalR) = aTotalt t2                
         in  (aprvdL + aprvdR, 1 + totalL + totalR)
