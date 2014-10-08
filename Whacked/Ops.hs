module Whacked.Ops where


data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | CmpLt
  | CmpGt
  deriving (Eq, Ord, Show)


data UnaryOp
  = Neg
  | Aot
  deriving (Eq, Ord, Show)
