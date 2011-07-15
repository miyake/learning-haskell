module Segment where

import qualified Ratio as R


data Point         = Point { x, y :: Int }
data Segment       = Segment { startPoint, endPoint :: Point }
data PointRational = PointRational { xRational, yRational :: R.Ratio Int }

instance Show Point where
  show p = "(" ++ show (x p) ++ ", " ++ show (y p) ++ ")"

instance Show Segment where
  show seg = show (startPoint seg) ++ " -> " ++ show (endPoint seg)

instance Show PointRational where
  show p = "(" ++ show (xRational p) ++ ", " ++ show (yRational p) ++ ")"

middle :: Segment -> PointRational
middle (Segment p0 p1) = PointRational midX midY
  where
    midX = (x p0 + x p1) R.% 2
    midY = (y p0 + y p1) R.% 2


p0 = Point 1 2
p1 = Point 3 5
seg = Segment p0 p1

main = do
  putStrLn $ show p0
  putStrLn $ show p1
  putStrLn $ show seg
  putStrLn $ show $ middle seg

