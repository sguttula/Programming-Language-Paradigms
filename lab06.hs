addInt :: Int -> Int -> Int
addInt i j = i + j

addInt' :: Int -> Int
addInt' = addInt 10

lowerCase :: Char -> Bool
lowerCase = (`elem` ['a' .. 'z'])

checkLower :: Char -> Bool
checkLower a = lowerCase a

functionInt :: (Int -> Int) -> Int -> Int
functionInt p q = p (p (p q))

threeFunctions :: Int -> Int -> Int -> Int
threeFunctions o k m = o * k * m

zip' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zip' _ [] _ _ = []  
zip' _ _ [] _ = []
zip' _ _ _ [] = []
zip' f (x:xs) (y:ys) (z:zs) = f x y z : zip' f xs ys zs
