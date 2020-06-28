leetCode [] = []
leetCode (x:xs) = 
     if x == 'o' 
     then '0' : leetCode xs 
     else if x == 'e'
     then '3' : leetCode xs
     else if x == 'a'
     then '@' : leetCode xs
     else if x == 'l'
     then '1' : leetCode xs
     else x : leetCode xs
