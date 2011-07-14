
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y >  x]

sortUniq :: Ord a => [a] -> [a]
sortUniq [] = []
sortUniq (x:xs) = sortUniq smaller ++ [x] ++ sortUniq larger
  where
    smaller = [y | y <- xs, y < x]
    larger  = [y | y <- xs, y > x]


-- test

list = [3,2,4,3,2,1,4,1]

test1 = [1,1,2,2,3,3,4,4] == (sort list)
test2 = [1,2,3,4] == (sortUniq list)

showResult True  = "OK"
showResult False = "NG"


main = do
  putStrLn $ showResult test1
  putStrLn $ showResult test2

