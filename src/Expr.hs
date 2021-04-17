module Expr where

import Lib

data Expr = Var
          | Const Complex
          | Sum Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving Show

