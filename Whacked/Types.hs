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
  | Cmp CondOp
  deriving ( Eq, Ord, Show )


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


getComparator :: (Ord a, Eq a) => CondOp -> (a -> a -> Bool)
getComparator op
  = \x y -> (x `compare` y) `elem` case op of
    CLT  -> [LT]
    CLTE -> [LT, EQ]
    CGT  -> [GT]
    CGTE -> [GT, EQ]
    CEQ  -> [EQ]
    CNEQ -> [LT, GT]


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