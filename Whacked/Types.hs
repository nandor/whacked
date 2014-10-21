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
  = Int
  | Bool
  | String
  | Char
  | Poly
  | Void
  | Array Type Int
  | Pair Type Type
  deriving ( Eq, Ord, Show )


binOpType :: BinaryOp -> Type -> Type -> Maybe Type
binOpType binOp lt rt
  = Map.lookup (binOp, lt, rt) . Map.fromList $
    [ ((Add,      Int,    Int),    Int )
    , ((Sub,      Int,    Int),    Int )
    , ((Div,      Int,    Int),    Int )
    , ((Mul,      Int,    Int),    Int )
    , ((Div,      Int,    Int),    Int )
    , ((Mod,      Int,    Int),    Int )
    , ((And,      Int,    Int),    Int )
    , ((And,      Bool,   Bool),   Bool)
    , ((Or,       Bool,   Bool),   Bool)
    , ((Cmp CLT,  Int,    Int),    Bool)
    , ((Cmp CLT,  Char,   Char),   Bool)
    , ((Cmp CLT,  String, String), Bool)
    , ((Cmp CLTE, Int,    Int),    Bool)
    , ((Cmp CLTE, Char,   Char),   Bool)
    , ((Cmp CLTE, String, String), Bool)
    , ((Cmp CGT,  Int,    Int),    Bool)
    , ((Cmp CGT,  Char,   Char),   Bool)
    , ((Cmp CGT,  String, String), Bool)
    , ((Cmp CGTE, Int,    Int),    Bool)
    , ((Cmp CGTE, Char,   Char),   Bool)
    , ((Cmp CGTE, String, String), Bool)
    , ((Cmp CEQ,  Int,    Int),    Bool)
    , ((Cmp CEQ,  Char,   Char),   Bool)
    , ((Cmp CEQ,  String, String), Bool)
    , ((Cmp CNEQ, Int,    Int),    Bool)
    , ((Cmp CNEQ, Char,   Char),   Bool)
    , ((Cmp CNEQ, String, String), Bool)
    ]


unOpType :: UnaryOp -> Type -> Maybe Type
unOpType unOp t
  = Map.lookup (unOp, t) . Map.fromList $
    [ ((Neg,   Int   ), Int )
    , ((Not,   Bool  ), Bool)
    , ((Ord,   Int   ), Char)
    , ((ToInt, Char  ), Int )
    , ((Len,   String), Int )
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