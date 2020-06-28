findNext :: Int -> [Int] -> Int
findNext i (k:xs)
    |length (k:xs) == 1 = -1
    |i == k = head xs
    |otherwise = findNext i xs

intDigitFunction :: Int -> Int
intDigitFunction 0 = 0
intDigitFunction a = a `mod` 10 + intDigitFunction b where b = digitSumCalculation a
digitSumCalculation :: Int -> Int
digitSumCalculation a = a `div` 10
digitSumCalculation 0 = 0

findPrev :: Int -> [Int] -> Int
findPrev j (p:q:xs)
    |length (p:q:xs) == 1 = -1
    |j == q = p
    |otherwise = findPrev j (q:xs)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
