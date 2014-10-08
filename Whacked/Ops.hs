module Whacked.Ops where


data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Eq, Ord, Show)


data UnaryOp
  = Neg
  | Aot
  deriving (Eq, Ord, Show)
