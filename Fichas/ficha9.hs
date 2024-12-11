-- // FICHA 9 \\ -- 

import System.Random  

-- Exemple 
-- Guess the number 

numb :: IO () 
numb = do r <- randomRIO (1,20)   
          putStrLn "Guess a number between 1 and 20: "
          x <- play r 1
          putStrLn $ "Got it in " ++ show x ++ " try's!"

play :: Int -> Int -> IO Int 
play r n = do s <- getLine
              v <- readIO s 
              if   v == r 
              then return n  
              else if   v > r 
                   then do putStrLn "Lower" 
                           play r (n + 1)    
                   else do putStrLn "Bigger" 
                           play r (n + 1) 

-- Basics Monad IO 

-- a) 
-- Gives the age and name of the user 

nameAge :: IO () 
nameAge = do putStrLn "Write your name: "
             name <- getLine 
             putStrLn "Write your age: " 
             age <- getLine 
             putStrLn $ "Hello " ++ name ++ "! You are " ++ age ++ " years old."  
             
-- b) 
-- Sums two given number 

sumIO :: IO () 
sumIO = do putStrLn "Introduce the first number: " 
           n1 <- getLine 
           putStrLn "Introduce the second number: "
           n2 <- getLine 
           r  <- plus (read n1) (read n2) 
           putStrLn $ "The sum of " ++ n1 ++ " with " ++ n2 ++ " is " ++ show r      

plus :: Int -> Int -> IO Int 
plus x y = return $ x + y 

-- c) 
-- Counts the number of words of a sentence 

nbrWords :: IO () 
nbrWords = do putStrLn "Write the sentence: "
              s   <- getLine 
              let w = words s 
              nrw <- word w  
              putStrLn $ "The sentence has " ++ show nrw ++ " words."  

word :: [String] -> IO Int 
word s = return $ length s  

-- d) 
-- Reverse a setence given by the user 

revStrg :: IO () 
revStrg = do putStrLn "Write the sentence: "
             s  <- getLine 
             rs <- rev s  
             putStrLn $ "The reversed setense is " ++ rs  

rev :: String -> IO String 
rev s = return $ reverse s 