
search :: (Double -> Double) -> Double -> Double
search f firstGuess = try firstGuess
  where
    try :: Double -> Double
    try guess
      | closeEnough guess next = next
      | otherwise              = try next
      where
        next = f guess

closeEnough :: Double -> Double -> Bool
closeEnough v1 v2 = abs (v1 - v2) < tolerance
  where tolerance = 0.001

sqrt :: Double -> Double
sqrt x = search (\y -> average y (x / y)) 1.0

average :: Double -> Double -> Double
average x y = (x + y) / 2.0

main = do
  putStrLn $ show $ search sin 1.0
  putStrLn $ show $ search cos 1.0
  putStrLn $ show $ search (\x -> sin x + cos x) 1.0
  putStrLn ""
  putStrLn $ "sqrt 4.0 = " ++ (show (Main.sqrt 4.0))


