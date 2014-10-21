module Whacked.Types where

import           Data.Map(Map)
import qualified Data.Map as Map


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
  | Poly
  | Array Type Int
  | Pair Type Type
  deriving ( Eq, Ord, Show )


binOpType :: Map BinaryOp [(Type, Type)]
binOpType
  = Map.fromList
    [ (Add,      [(Int, Int)])
    , (Sub,      [(Int, Int)])
    , (Mul,      [(Int, Int)])
    , (Div,      [(Int, Int)])
    , (Mod,      [(Int, Int)])
    , (And,      [(Bool, Bool)])
    , (Or,       [(Bool, Bool)])
    , (Cmp CLT,  [(Int, Int), (Char, Char), (String, String)])
    , (Cmp CLTE, [(Int, Int), (Char, Char), (String, String)])
    , (Cmp CGT,  [(Int, Int), (Char, Char), (String, String)])
    , (Cmp CGTE, [(Int, Int), (Char, Char), (String, String)])
    , (Cmp CEQ,  [(Int, Int), (Char, Char), (String, String)])
    , (Cmp CNEQ, [(Int, Int), (Char, Char), (String, String)])
    ]


getComparator :: (Ord a, Eq a) => CondOp -> (a -> a -> Bool)
getComparator op
  = \x y -> (x `compare` y) `elem` case op of
    CLT  -> [LT]
    CLTE -> [LT, EQ]
    CGT  -> [GT]
    CGTE -> [GT, EQ]
    CEQ  -> [EQ]
    CNEQ -> [LT, GT]


match :: Type -> Type -> Bool
match Poly (Pair _ _)
  = True
match (Pair _ _) Poly
  = True
match x y
  | x == y = True
  | otherwise = case (x, y) of
    (Pair x y, Pair x' y') -> x `match` x' && y `match` y'
    _ -> False


elemType :: Type -> Type
elemType (Array t 1)
  = t
elemType (Array t n)
  = Array t (n - 1)


isReadable :: Type -> Bool
isReadable Void
  = False
isReadable (Pair _ _)
  = False
isReadable (Array _ _)
  = False
isReadable Bool
  = False
isReadable _
  = True