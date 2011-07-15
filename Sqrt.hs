
-- Square root by Newton's method

sqrt :: Double -> Double -> Double
sqrt x tolerance = iter 1.0 x

  where
    iter :: Double -> Double -> Double
    iter guess x
      | goodEnough guess x = guess
      | otherwise          = iter (improve guess x) x

    improve :: Double -> Double -> Double
    improve guess x = average guess (x / guess)

    average :: Double -> Double -> Double
    average x y = (x + y) / 2.0

    goodEnough :: Double -> Double -> Bool
    goodEnough guess x = abs (guess * guess - x) < tolerance


main = do
  putStrLn $ show $ Main.sqrt 4.0 0.1
  putStrLn $ show $ Main.sqrt 4.0 0.00001
  putStrLn $ show $ Main.sqrt 4.0 0.000000001
  putStrLn $ show $ Main.sqrt 4.0 0.000000000001
  putStrLn $ show $ Main.sqrt 4.0 0.000000000000001
  putStrLn $ show $ Prelude.sqrt 4.0


