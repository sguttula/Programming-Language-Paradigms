--1
isDivisible5 :: Int -> Bool 
isDivisible5 n 
 | n `mod` 5 == 0 = True 
 |otherwise = False
 
--6
listCall ::  [f]->[(f, f)]
listCall convertType = listCreation convertType []  

listCreation  ::[l]  -> [(l, l)] ->[(l, l)] 
listCreation [] letterFunc = letterFunc
listCreation [l]  letterFunc  = letterFunc
listCreation (x:y:z) letterFunc = (x, y):(listCreation z letterFunc)

--7
product  :: Num i => [i] -> [i]
product prod = foldl (\j (product1, product2) -> j ++ [product1 * product2]) [] $ listCall prod

--8
additionWithLists :: [Int] -> [Int]
additionWithLists [] = []
additionWithLists ints = tail $ scanl ( \firstInt nextInt -> nextInt + firstInt) 0 ints

--9
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = (f .f)  $   f x
 
--10
isLowerCase :: Char -> Bool
isLowerCase = flip elem  ['a' .. 'z']

--11
--Pack repeats in a list of Chars into separate lists, resulting in a sorted list of lists
charToString1 :: [Char] -> [String]
charToString1 c = filter ( \max -> max /= "" ) $ zipWith ( \sorted helpMethod -> filter ( \order -> order == helpMethod) sorted) (repeat c)  ['a' .. 'z']

helperMethod ::[(Int,[String])]-> [(Int,[String])] -> Int -> [String]
helperMethod [] switch m =  map ( \(a, b) -> unwords b ) switch
helperMethod interChange switch stringLength =  helperMethod ((filter ( \(n, m) -> n /= stringLength)) interChange) ( switch++ (filter ( \(n, m) -> n == stringLength) interChange)) (stringLength + 1)

-- Sort a list of Strings by length of the first word in the strings (if the strings have more than one word)
sortString :: [String] -> [String]
sortString  list = helperMethod (map ( \lists -> (length(head lists), lists)) (map words list)) [] 0