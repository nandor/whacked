module Whacked.Types where

import           Data.Bits



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
  deriving ( Eq, Ord )


instance Show BinaryOp where
  show Add     = "+"
  show Sub     = "-"
  show Mul     = "*"
  show Div     = "/"
  show Mod     = "%"
  show And     = "&"
  show Or      = "|"
  show (Cmp x) = show x


-- |Unary operators.
data UnaryOp
  = Neg
  | Not
  | Ord
  | Chr
  | Len
  deriving ( Eq, Ord )


instance Show UnaryOp where
  show Neg = "-"
  show Not = "!"
  show Ord = "ord"
  show Chr = "chr"
  show Len = "len"


-- |Comparision operators.
data CondOp
  = CLT
  | CLTE
  | CGT
  | CGTE
  | CEQ
  | CNE
  deriving ( Eq, Ord )


instance Show CondOp where
  show CLT  = "<"
  show CLTE = "<="
  show CGT  = ">"
  show CGTE = ">="
  show CEQ  = "=="
  show CNE  = "/="


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


-- |Returns the size in bytes of an element.
sizeof :: Type -> Int
sizeof (Int     ) = 4
sizeof (Bool    ) = 1
sizeof (Char    ) = 1
sizeof (Array _ ) = 4
sizeof (Pair _ _) = 8
sizeof (Poly    ) = 8
sizeof (Empty   ) = 4
sizeof (Null    ) = 4
sizeof (Void    ) = 0


-- Negates the comparison operator.
invertCond :: CondOp -> CondOp
invertCond CLT  = CGTE
invertCond CLTE = CGT
invertCond CGT  = CLTE
invertCond CGTE = CLT
invertCond CEQ  = CNE
invertCond CNE  = CEQ


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


-- |Return the type of an unary operation or nothing
unOpType :: UnaryOp -> Type -> Maybe Type
unOpType Neg Int         = Just Int
unOpType Not Bool      = Just Bool
unOpType Ord Char      = Just Int
unOpType Chr Int       = Just Char
unOpType Len (Array _) = Just Int
unOpType _ _           = Nothing


-- |Returns a function that compares two values.
compareOp :: (Ord a, Eq a) => CondOp -> a -> a -> Bool
compareOp op x y
  = (x `compare` y) `elem` case op of
    CLT  -> [LT]
    CLTE -> [LT, EQ]
    CGT  -> [GT]
    CGTE -> [GT, EQ]
    CEQ  -> [EQ]
    CNE  -> [LT, GT]


-- |Checks if two types match.
match :: Type -> Type -> Bool
match (Null    ) (Poly    ) = True
match (Poly    ) (Null    ) = True
match (Null    ) (Pair _ _) = True
match (Pair _ _) (Null    ) = True
match (Null    ) (Array _ ) = True
match (Array _ ) (Null    ) = True
match (Poly    ) (Pair _ _) = True
match (Pair _ _) (Poly    ) = True
match (Empty   ) (Array _ ) = True
match (Array _ ) (Empty   ) = True
match (Array x ) (Array y ) = match x y
match (Pair x y) (Pair z v) = match x z && match y v
match (x       ) (y       ) = x == y


-- |Checks if a RValue can be read.
isReadable :: Type -> Bool
isReadable Int          = True
isReadable Char         = True
isReadable (Array Char) = True
isReadable _            = False



-- |Checks if the variable can be encoded in an ARM instruction.
fitsInImm :: Int -> Bool
fitsInImm x
  = ((x `shiftR` 16) .&. 0xFFFFFF00) == 0
