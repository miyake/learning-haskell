module Rational where


data Rational = Rational { numerator, denominator :: Int }

instance Show Rational.Rational where
  show (Rational.Rational numer denom) = show numer ++ "/" ++ show denom

instance Eq Rational.Rational where
  (Rational.Rational n1 d1) == (Rational.Rational n2 d2) =
    n1 * d2 == n2 * d1

instance Num Rational.Rational where
  (Rational.Rational n1 d1) + (Rational.Rational n2 d2) =
    Rational.Rational (n1 * d2 + n2 * d1) (d1 * d2)

  (Rational.Rational n1 d1) * (Rational.Rational n2 d2) =
    Rational.Rational (n1 * n2) (d1 * d2)

  abs (Rational.Rational n d) = Rational.Rational (abs n) (abs d)

  signum (Rational.Rational n d)
    | d == 0               = error "zero denominator"
    | n == 0               = Rational.Rational 0 1
    | signum n == signum d = Rational.Rational 1 1
    | otherwise            = Rational.Rational (-1) 1

  fromInteger i = Rational.Rational (fromInteger i) 1

instance Ord Rational.Rational where
  (Rational.Rational n1 d1) < (Rational.Rational n2 d2) =
    n1 * d2 < n2 * d1


main = do
  putStrLn $ show $ fromInteger 3
  putStrLn $ show (Rational.Rational 1 2)
  putStrLn $ show $ (Rational.Rational 1 2) == (Rational.Rational 2 4)

