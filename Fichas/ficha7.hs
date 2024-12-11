-- // FICHA 7 \\ -- 

--   1. 

data ExpInt = Const Int
            | Simetric ExpInt
            | Plus     ExpInt ExpInt
            | Minus    ExpInt ExpInt
            | Mult     ExpInt ExpInt 

-- Exemplo 

exp1 = Plus (Const 3) (Minus (Const 2) (Const 5)) 

-- a) 
-- Claculates the expression of the tree 

calc :: ExpInt -> Int 
calc (Const x) 
    = x
calc (Simetric x) 
    = - calc x 
calc (Plus x y) 
    = calc x + calc y 
calc (Minus x y) 
    = calc x - calc y 
calc (Mult x y) 
    = calc x * calc y 

-- b) 
-- Writes the expression given by the tree 

write :: ExpInt -> String 
write (Const x) 
     = show x 
write (Simetric x) 
     = "- " ++ write x 
write (Plus x y) 
     = write x ++ " + " ++ write y 
write (Minus x y) 
     = write x ++ " - " ++ write y 
write (Mult x y) 
     = write x ++ " * " ++ write y 

-- c) 
-- Writes the expresion showing first the number and then the signals 

write' :: ExpInt -> String 
write' (Const x)
      = show x 
write' (Simetric x) 
      = " -" ++ write' x 
write' (Plus x y) 
      = write' x ++ " " ++ write' y ++ " +" 
write' (Minus x y) 
      = write' x ++ " " ++ write' y ++ " -"
write' (Mult x y) 
      = write' x ++ " " ++ write' y ++ " *" 


--   2. 

data RTree a = R a [RTree a] 
             deriving Show 

-- Exemples 

tree = R 9 [ R 8 [ R 1 [], R 2 [] ], R 7 [], R 6 [ R 3 [], R 4 [], R 5 [] ] ] 

-- a) 
-- Sums all elementes of the tree 

sumRT :: Num a => RTree a -> a 
sumRT (R x [])
     = x 
sumRT (R x (h:t)) 
     = x + sum (map sumRT (h:t)) 

-- b) 
-- Calculates the heigh of the tree 

heigth :: RTree a -> Int 
heigth (R x []) 
      = 1 
heigth (R x (h:t)) 
      = 1 + maximum (map heigth (h:t)) 

-- c) 
-- Removes all elementes of a tree when past a certain height 

cut :: Int -> RTree a -> RTree a 
cut 0 (R x _)  
   = R x [] 
cut _ (R x [])
   = R x [] 
cut y (R x (h:t))  
    = R x (map (cut (y - 1)) (h:t))  

-- d) 
-- Generates the simetric tree 

mirror :: RTree a -> RTree a 
mirror (R x []) 
      = R x []
mirror (R x p) 
      = R x (reverse $ map mirror p) 

-- c) 
-- Generates a list of the elementes of a tree (first the leafs then the root)

postOrd :: RTree a -> [a] 
postOrd (R x []) 
       = [x]    
postOrd (R x (h:t)) 
       = unlist (map postOrd (h:t)) ++ [x] 
  where
       unlist :: [[a]] -> [a]
       unlist  [] 
             = []  
       unlist (h : t)
             = h ++ unlist t    


-- 3. 

data LTree a = Tip a 
             | Fork (LTree a) (LTree a) 
             deriving Show  

-- Exemple 

ltTree = Fork (Fork (Tip 1) (Tip 2)) (Tip 3)  

-- a) 
-- Sums the leafs of a tree 

ltSum :: Num a => LTree a -> a 
ltSum (Tip x)  
     = x 
ltSum (Fork t1 t2) 
     = ltSum t1 + ltSum t2  

-- b) 
-- Gives the list of the leafs of the tree from left to rigth 

listLT :: LTree a -> [a] 
listLT (Tip x) 
      = [x] 
listLT (Fork t1 t2) 
      = listLT t1 ++ listLT t2 

-- c) 
-- Calculat the heigth of the tree

ltHeigth :: LTree a -> Int 
ltHeigth (Tip x) 
        = 1 
ltHeigth (Fork t1 t2) 
        = 1 + max (ltHeigth t1) (ltHeigth t2)  


--   4. 

data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)
               deriving Show 

data BTree a = Empty 
             | Node a (BTree a) (BTree a) 
             deriving Show
             

-- Exemple 

ftree = No 1 (No 2 (Leaf 'I') (Leaf 'J')) (Leaf 'L') 

btree = Node 1 (Node 2 Empty Empty) Empty 

-- a) 
-- Separates the info in a full tree into two trees 

splitFTree :: FTree a b -> (BTree a, LTree b) 
splitFTree tree 
          = (no tree, leaf tree) 
     where
          no (Leaf _) 
            = Empty 
          no (No x t1 t2) 
            = Node x (no t1) (no t2) 
          -- 
          leaf (Leaf x) 
              = Tip x 
          leaf (No _ t1 t2) 
              = Fork (leaf t1) (leaf t2)     

-- b)
-- joins two trees into a full tree 

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b) 
joinTrees Empty (Tip x) 
         = Just $ Leaf x 
joinTrees (Node x t1 t2) (Fork t1' t2') 
         | empt (Node x t1 t2) == tip (Fork t1' t2')  
          = Just $ No x (join t1 t1') (join t2 t2') 
         | otherwise 
          = Nothing 
    where
         empt Empty 
             = 1 
         empt (Node _ t1 t2) 
             = empt t1 + empt t2 
         -- 
         tip (Tip _)
            = 1
         tip (Fork t1 t2) 
            = tip t1 + tip t2
         --         
         join Empty (Tip x) 
             = Leaf x 
         join (Node x t1 t2) (Fork t1' t2') 
             = No x (join t1 t1') (join t2 t2') 
joinTrees _ _
         = Nothing     
  