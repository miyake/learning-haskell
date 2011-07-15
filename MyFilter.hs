module MyFilter where


type Filter a = (a -> Bool) -> [a] -> [a]

usingListComprehension :: Filter a
usingListComprehension p xs = [x | x <- xs, p x]

recursive :: Filter a
recursive p [] = []
recursive p (x:xs)
  | p x       = x : (recursive p xs)
  | otherwise = recursive p xs

usingFoldr :: Filter a
usingFoldr p = foldr ((++).(\ x -> if p x then [x] else [])) []


main = do
  putStrLn $ show $ usingListComprehension even [0..10]
  putStrLn $ show $ recursive              even [0..10]
  putStrLn $ show $ usingFoldr             even [0..10]

