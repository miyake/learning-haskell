module MyMap where


type Map a b = (a -> b) -> [a] -> [b]

usingListComprehension :: Map a b
usingListComprehension f xs = [f x | x <- xs]

recursive :: Map a b
recursive f []     = []
recursive f (x:xs) = (f x) : (recursive f xs)

usingFoldr :: Map a b
usingFoldr f = foldr ((:).f) []


main = do
  putStrLn $ show $ usingListComprehension (1+) [0..2]
  putStrLn $ show $ recursive              (1+) [0..2]
  putStrLn $ show $ usingFoldr             (1+) [0..2]

