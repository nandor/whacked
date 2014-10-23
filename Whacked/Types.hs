module Whacked.Types where

import Debug.Trace
-- |Binary operators.
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


-- |Unary operators.
data UnaryOp
  = Neg
  | Not
  | Ord
  | Chr
  | Len
  deriving ( Eq, Ord, Show )


-- |Comparision operators.
data CondOp
  = CLT
  | CLTE
  | CGT
  | CGTE
  | CEQ
  | CNE
  deriving ( Eq, Ord, Show )


-- |All types present in the WACC language.
data Type
  = Int
  | Bool
  | Char
  | Array Type
  | Pair Type Type
  | Poly
  | Empty
  | Null
  | Void
  deriving ( Eq, Ord, Show )


-- |Pair fields.
data Elem
  = Fst
  | Snd
  deriving ( Eq, Ord, Show )


-- |Returns the type of the result of a binary operation or Nothing.
binOpType :: BinaryOp -> Type -> Type -> Maybe Type
binOpType Add Int  Int  = Just Int
binOpType Sub Int  Int  = Just Int
binOpType Div Int  Int  = Just Int
binOpType Mul Int  Int  = Just Int
binOpType Mod Int  Int  = Just Int
binOpType And Bool Bool = Just Bool
binOpType Or  Bool Bool = Just Bool

binOpType (Cmp CLT ) Int  Int  = Just Bool
binOpType (Cmp CLT ) Char Char = Just Bool
binOpType (Cmp CLTE) Int  Int  = Just Bool
binOpType (Cmp CLTE) Char Char = Just Bool
binOpType (Cmp CGT ) Int  Int  = Just Bool
binOpType (Cmp CGT ) Char Char = Just Bool
binOpType (Cmp CGTE) Int  Int  = Just Bool
binOpType (Cmp CGTE) Char Char = Just Bool

binOpType (Cmp CEQ) Int  Int  = Just Bool
binOpType (Cmp CEQ) Char Char = Just Bool
binOpType (Cmp CEQ) Bool Bool = Just Bool
binOpType (Cmp CNE) Int  Int  = Just Bool
binOpType (Cmp CNE) Char Char = Just Bool
binOpType (Cmp CNE) Bool Bool = Just Bool

binOpType (Cmp CEQ) (Array _ ) (Array _ ) = Just Bool
binOpType (Cmp CEQ) (Pair _ _) (Pair _ _) = Just Bool
binOpType (Cmp CEQ) (Pair _ _) (Null    ) = Just Bool
binOpType (Cmp CEQ) (Null    ) (Pair _ _) = Just Bool
binOpType (Cmp CNE) (Array _ ) (Array _ ) = Just Bool
binOpType (Cmp CNE) (Pair _ _) (Pair _ _) = Just Bool
binOpType (Cmp CNE) (Pair _ _) (Null    ) = Just Bool
binOpType (Cmp CNE) (Null    ) (Pair _ _) = Just Bool

binOpType _ _ _
  = Nothing


-- Return the type of an unary operation or nothing
unOpType :: UnaryOp -> Type -> Maybe Type
unOpType Neg Int         = Just Int
unOpType Not Bool      = Just Bool
unOpType Ord Char      = Just Int
unOpType Chr Int       = Just Char
unOpType Len (Array _) = Just Int
unOpType _ _           = Nothing



getComparator :: (Ord a, Eq a) => CondOp -> (a -> a -> Bool)
getComparator op
  = \x y -> (x `compare` y) `elem` case op of
    CLT  -> [LT]
    CLTE -> [LT, EQ]
    CGT  -> [GT]
    CGTE -> [GT, EQ]
    CEQ  -> [EQ]
    CNE  -> [LT, GT]


match :: Type -> Type -> Bool
match (Null    ) (Pair _ _) = True
match (Pair _ _) (Null    ) = True
match (Poly    ) (Pair _ _) = True
match (Pair _ _) (Poly    ) = True
match x y
  | x == y = True
  | otherwise = case (x, y) of
    (Pair x y, Pair x' y') -> x `match` x' && y `match` y'
    _ -> False


isReadable :: Type -> Bool
isReadable Void
  = False
isReadable (Pair _ _)
  = False
isReadable (Array _)
  = False
isReadable Bool
  = False
isReadable _
  = True