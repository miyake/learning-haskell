
execute :: (Double -> Double) -> Double -> Double -> Double
execute f a b
  | (fa < 0.0) && (fb > 0.0) = search f a b
  | (fa > 0.0) && (fb < 0.0) = search f b a
  | otherwise                = error $ "Values are not of oposite sign: " ++ show a ++ ", " ++ show b
  where
    fa = f a
    fb = f b

search :: (Double -> Double) -> Double -> Double -> Double
search f negPoint posPoint
  | closeEnough negPoint posPoint = midPoint
  | testValue > 0.0               = search f negPoint midPoint
  | testValue < 0.0               = search f midPoint posPoint
  | otherwise                     = midPoint
  where
    midPoint  = average negPoint posPoint
    testValue = f midPoint

    closeEnough :: Double -> Double -> Bool
    closeEnough x y = abs (x - y) < tolerance
      where tolerance = 0.001

average :: Double -> Double -> Double
average x y = (x + y) / 2.0


main = do
  putStrLn $ show $ execute (\x -> x * x * x - 2.0 * x - 3.0) 1.0 2.0
  putStrLn $ show $ execute sin (-1.0) 1.0
  putStrLn $ show $ execute sin 1.0 (-1.0)
  putStrLn $ show $ execute sin 1.0 5.0
  putStrLn $ show $ execute sin 1.0 2.0

