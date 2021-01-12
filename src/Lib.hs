module Lib
    ( someFunc
    , Complex (Cartesian, Polar)
    ) where

type Re = Double
type Im = Double

type Norm = Double
type Argument = Double

data Complex = Cartesian Re Im
             | Polar Norm Argument

instance Show Complex where
  show (Cartesian x y) = (show x) ++ " + " ++ (show y) ++ "i"
  show (Polar     r t) = (show r) ++ "e^(" ++ (show t) ++ "i)"

mod2PI :: Argument -> Argument
mod2PI t
  | (t <= 2*pi) && (t >= 0) = t
  | (t >= 2*pi)             = mod2PI (t - 2*pi)
  | (t < 0)                 = mod2PI (t + 2*pi)

toCartesian :: Complex -> Complex
toCartesian (Cartesian x y) = Cartesian x y
toCartesian (Polar r t) = undefined

toPolar :: Complex -> Complex
toPolar (Cartesian x y) = undefined
toPolar (Polar r t) = (Polar r t)

conjugate :: Complex -> Complex
conjugate (Cartesian x y) = Cartesian x (-y)
conjugate (Polar r t) = Polar r (-t)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
