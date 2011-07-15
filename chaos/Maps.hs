module Maps where


type Map1d = Double -> Double

logistic :: Map1d
logistic x = 4.0 * x * (1.0 - x)

tent :: Map1d
tent x
  | 0.0 <= x && x <= 0.5 = 2.0 * x
  | 0.5 <  x && x <= 1.0 = 2.0 - 2.0 * x
  | otherwise            = error "Maps.tent: range error"

bernoulli :: Map1d
bernoulli x
  | 0.0 <= x && x <= 0.5 = 2.0 * x
  | 0.5 <  x && x <= 1.0 = 2.0 * x - 1.0
  | otherwise            = error "Maps.bernoulli: range error"

nathanson :: Integer -> Double -> Map1d
nathanson p delta
  | p < 3                             = error "Maps.nathanson: the first argument must be greater than or equal to 3."
  | delta <= 0.0                      = error "Maps.nathanson: the second argument must be positive."
  | 2.0 ** (- fromInteger p) <= delta = error "Maps.nathanson: the first and second argument must meet the constraint."
  | otherwise =
      \x ->
        if      0.0 <= x && x < 1.0 - pinv then x + pinv
        else if x < 1.0 - pinv + delta     then 1.0 - (1.0 - delta) / delta * (x - 1.0 + pinv)
        else if x <= 1.0                   then x - 1.0 + pinv
        else error "Maps.nathanson: argument must be in (0.0, 1.0)."
          where pinv = 1.0 / (fromInteger p)


main = do
  putStrLn $ show $ logistic 0.5
  putStrLn $ show $ tent 0.5
  putStrLn $ show $ bernoulli 0.5
  putStrLn $ show $ nathanson 3 0.1 0.5

