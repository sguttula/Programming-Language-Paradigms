function1 x y = x^2 + y^2

function2 i = i + i

function3 j = j^2 + j

typeclass :: Num -> Num
typeclass a = a + a 

typeclass2 :: (String a, Float b)
typeclass2 q = a + b

ints :: (Int a) => a -> String
ints 1 = "Once"
ints 2 = "Twice"
ints 3 = "Thrice"
ints x = "Don't know how to say that in English."

exponent :: (Int a) => a -> a
exponent 0 = 1
exponent n = n^2
