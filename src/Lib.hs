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

instance Semigroup Complex where
  (<>) = add

instance Monoid Complex where
  mempty = Cartesian 0 0

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

arg :: Complex -> Argument
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

add :: Complex -> Complex -> Complex
add (Cartesian x y) (Cartesian x' y') = Cartesian (x + x') (y + y')
add (Polar r t) (Polar r' t') = Polar newr newt
  where 
    newr = sqrt(r*r + r'*r' + 2*r*r')*cos(t' - t)
    newt = t + arg (Cartesian (r' * sin(t' - t)) (r + r' * cos(t' - t)))
add z1@(Cartesian _ _) z2@(Polar _ _) = add z1 $ toCartesian z2
add z1@(Polar _ _) z2@(Cartesian _ _) = add z1 $ toPolar z2

multiply :: Complex -> Complex -> Complex
multiply (Cartesian x y) (Cartesian x' y') = Cartesian (x*x' - y*y') (x*y' + x'*y)
multiply (Polar r t) (Polar r' t') = Polar (r*r') (t + t')
multiply z1@(Cartesian _ _) z2@(Polar _ _) = multiply z1 $ toCartesian z2
multiply z1@(Polar _ _) z2@(Cartesian _ _) = multiply z1 $ toPolar z2

someFunc :: IO ()
someFunc = putStrLn "someFunc"

