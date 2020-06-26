generation :: Int -> String
generation i 
    | i > 1995 = "Generation Z"  
    | i > 1980 = "Millennial" 
    | i > 1965 = "Generation X"
    | i > 1945 = "Baby Boomer"
    | i > 1933 = "The Silent Generation"
    | otherwise   = "The Greatest Generation"
						
generation2 :: Int -> String
generation2 a
    | i > 1995 = "Generation Z"  
    | i > 1980 = "Millennial" 
    | i > 1965 = "Generation X"
    | i > 1945 = "Baby Boomer"
    | i > 1933 = "The Silent Generation"
    | otherwise   = "The Greatest Generation"	
     where
		i = 2017 - a
	
letclause ::  Float -> [Float] -> [Float] 
letclause r h = 
    let w = pi * r    
    in  [b * w | b <- h] 
	
