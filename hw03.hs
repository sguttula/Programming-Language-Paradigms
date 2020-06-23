import Data.List
import System.Environment   
import System.Directory  
import System.IO  

data Person = Person { firstName :: String, lastName :: String, major :: String, age :: Int}
 deriving (Show)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
 
csv :: a -> Tree a
csv b = Node b EmptyTree EmptyTree

ins :: (Ord a) => a -> Tree a -> Tree a  
ins nde EmptyTree = csv nde  
ins nde (Node a left right)   
    | nde == a = Node nde left right  
    | nde < a  = Node a (ins nde left) right  
    | nde > a  = Node a left (ins nde right)  

sea :: (Ord a) => a -> Tree a -> Bool
sea nde EmptyTree = False  
sea nde (Node a left right)  
    | nde == a = True  
    | nde < a  = sea nde left  
    | nde > a  = sea nde right
 
before :: String -> String -> Bool
before [] star = True
before (g:qwer) [] = False
before (g:qwer) (q:star) = (g == q) && before qwer star

tf :: String -> String -> Bool
tf (g:qwer) [] = False
tf qwer star
    | before qwer star = True
    | tf qwer (tail star) = True
    | otherwise = False

main = do
    students <- readFile "hw3.csv"
    let boo = lines students
        ness = zipWith (\tabs line -> show tabs ++ " - " ++ line) [0..] boo
        classes = foldr ins EmptyTree boo
    putStrLn "Search a student age:"
    num <- getLine
    putStrLn "Search a student name:"
    name <- getLine
    let vaysu = tf num students
        peru = tf name students
    putStr $ unlines ness
    putStr "Student's age : "
    putStrLn $ (show vaysu)
    putStr "Student's name : "
    putStr $ (show peru)
  
