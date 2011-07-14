
recursive :: Int -> Int
recursive n
  | n == 0    = 1
  | n == 1    = 1
  | otherwise = recursive (n - 1) + recursive (n - 2)

iterative :: Int -> Int
iterative n = inner 1 0 n
  where
    inner :: Int -> Int -> Int -> Int
    inner n1 n2 count
      | count == 0 = n1
      | otherwise  = inner (n1 + n2) n1 (count - 1)


main = do
  putStrLn $ show $ recursive 10
  putStrLn $ show $ iterative 10

