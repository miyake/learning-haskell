
recursive :: Int -> Int
recursive
  | n == 1    = 1
  | otherwise = n * (recursive (n - 1))

iterative :: Int -> Int
iterative n = inner 1 1 n
  where
    inner :: Int -> Int -> Int -> Int
    inner product counter maxCount
      | counter > maxCount = product
      | otherwise          = inner (counter * product) (counter + 1) maxCount

usingFoldr :: Int -> Int
usingFoldr n = foldr (*) 1 [1..n]

usingFoldl :: Int -> Int
usingFoldl n = foldl (*) 1 [1..n]


main = do
  putStrLn $ show $ recursive 10
  putStrLn $ show $ iterative 10
  putStrLn $ show $ usingFoldr 10
  putStrLn $ show $ usingFoldl 10


