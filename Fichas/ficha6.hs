-- // FICHA 6 \\ -- 

-- 1.

data BTree a = Empty 
             | Node a (BTree a) (BTree a) 
             deriving Show 

--   a)
-- Calculates the heigth of a tree 
heigth :: BTree a -> Int 
heigth Empty 
      = 0 
heigth (Node x n1 n2) 
      = 1 + max (heigth n1) (heigth n2) 

--   b) 
-- Calculates the number of nodes in a tree 
countNodes :: BTree a -> Int 
countNodes Empty 
          = 0 
countNodes (Node x n1 n2)
          = 1 + countNodes n1 + countNodes n2 

--   c) 
-- Calculates the number of leaves of a tree 
leaves :: BTree a -> Int 
leaves Empty 
      = 0 
leaves (Node x Empty Empty) 
      = 1 
leaves (Node x n1 n2) 
      = leaves n1 + leaves n2 

--   d) 
-- After a certain heigth eliminates the elementes of the tree 
lumberJ :: Int -> BTree a -> BTree a 
lumberJ _ Empty 
       = Empty 
lumberJ 0 _ 
       = Empty        
lumberJ h (Node x n1 n2) 
       = Node x (lumberJ (h-1) n1) (lumberJ (h-1) n2) 

--   e) 
-- Folows a given path and gives the list of info from that path 
-- True = Right | False = Left 
path :: [Bool] -> BTree a -> [a] 
path  [] _ 
    = [] 
path  _ Empty  
    = []
path (h:t) (Node x n1 n2) 
    | h == True  = x : path t n2 
    | otherwise  
     = x : path t n1 

--   f) 
-- Creates the simetric of a given tree 
mirror :: BTree a -> BTree a 
mirror  Empty 
      = Empty 
mirror (Node x n1 n2) 
      = Node x (mirror n2) (mirror n1)  
       
--   g) 
-- Sums two trees 
--zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c 
--zipWithBT   
