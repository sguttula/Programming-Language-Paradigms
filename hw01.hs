import Data.Set

recursive1 :: Int -> Double
recursive1 0 = 0
recursive1 n = (1 / fromIntegral(n)) + recursive1 (n - 1)

recursive2 :: Int -> Double
recursive2 0 = 0
recursive2 a = (fromIntegral(a) / fromIntegral(a + 1) + recursive2(a - 1))

intMaxElimination :: Eq i => [i] -> [i]
intMaxElimination = max []
    where max dupl [] = dupl
          max dupl (x:xs)
              | x `elem` dupl = max dupl xs
              | otherwise = max (dupl ++ [x]) xs