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
  | Not
  | Ord
  | ToInt
  | Fst
  | Snd
  | Len
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
  | Array Type Int
  | Pair (Maybe Type) (Maybe Type)
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