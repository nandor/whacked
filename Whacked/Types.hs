module Whacked.Types where

-- |Basic types that are shared between the AST and the intermediate forms.



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
  | CmpLte
  | CmpGte
  | CmpEq
  | CmpNeq
  deriving ( Eq, Ord, Show )


isComparison :: BinaryOp -> Bool
isComparison x
  = x `elem` [CmpLt, CmpGt, CmpLte, CmpGte, CmpEq, CmpNeq]


data UnaryOp
  = Neg
  | Aot
  deriving ( Eq, Ord, Show )


data CondOp
  = CLT
  | CLTE
  | CGT
  | CGTE
  | CEQ
  | CNEQ
  deriving ( Eq, Ord, Show )


data Type
  = Void
  | Int
  | Bool
  | String
  | Char
  | Real
  | Tuple Type Type
  | Array Type [Int]
  deriving ( Eq, Ord, Show )


data TupleField
  = First
  | Second
  deriving (Eq, Ord, Show)