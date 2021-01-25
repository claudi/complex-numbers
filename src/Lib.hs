module Lib
    ( someFunc
    , Complex (..)
    ) where

type Re = Double
type Im = Double

type Norm = Double
type Argument = Double

data Complex = Cartesian Re Im
             | Polar Norm Argument

instance Show Complex where
  show (Cartesian x y) = show x ++ " + " ++ show y ++ "*i"
  show (Polar     r t) = show r ++ "e^(" ++ show t ++ "*i)"

mod2PI :: Argument -> Argument
mod2PI t
  | t <= 2*pi && t >= 0 = t
  | t >= 2*pi             = mod2PI (t - 2*pi)
  | t < 0                 = mod2PI (t + 2*pi)

simplifyNum :: Complex -> Complex
simplifyNum (Cartesian x y) = Cartesian x y
simplifyNum (Polar r t) = Polar r $ mod2PI t

norm :: Complex -> Double
norm (Cartesian x y) = sqrt(x*x + y*y)
norm (Polar r _) = r

arg :: Complex -> Double
arg (Cartesian x y) 
    | x == 0 = 0
    | x > 0  = atan (y/x)
    | x < 0  = atan $ (y/x) + pi
arg (Polar _ t) = t

toCartesian :: Complex -> Complex
toCartesian (Cartesian x y) = Cartesian x y
toCartesian (Polar r t) = Cartesian (r * cos t) (r * sin t)

toPolar :: Complex -> Complex
toPolar z@(Cartesian _ _) = Polar (norm z) (arg z)
toPolar   (Polar r t) = Polar r t

conjugate :: Complex -> Complex
conjugate (Cartesian x y) = Cartesian x (-y)
conjugate (Polar r t) = Polar r (-t)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

