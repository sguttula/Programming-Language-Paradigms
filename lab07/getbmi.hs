weightHeightTell :: (RealFloat a) => a -> a -> String  
weightHeightTell weight height  
--converted to kg and meters in the conversion below--
    | ((weight / 2.2046226218) / (height * 0.0254) ^ 2) <= underWeight = "You're underweight"  
    | ((weight / 2.2046226218) / (height * 0.0254) ^ 2) <= normalWeight = "You're supposedly normal"  
    | ((weight / 2.2046226218) / (height * 0.0254) ^ 2) <= overWeight = "You're overweight"  
    | otherwise                   = "You're obese"
   where underWeight = 18.5
         normalWeight = 25.0
         overWeight = 30.0
