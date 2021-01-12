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

conjugate :: Complex -> Complex
conjugate (Cartesian x y) = Cartesian x (-y)
conjugate (Polar r t) = Polar r (-t)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
