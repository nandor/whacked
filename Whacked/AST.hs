module Whacked.AST where


data ATag
  = ATag
    { atLine :: Int
    , atChar :: Int
    , atIndex :: Int
    }
  deriving (Eq, Ord, Show)

data AProgram
  = AProgram
    { apFunctions :: [AFunction]
    , apMain :: ABlock
    , apTag :: ATag
    }
  deriving (Eq, Ord, Show)


data AFunction
  = AFunction
    { afArgs :: [AArg]
    , afReturn :: AType
    , afName :: String
    , afBody :: ABlock
    , afTag :: ATag
    }
  deriving (Eq, Ord, Show)


data AArg
  = AArg
    { aaTag :: ATag
    , aaType :: AType
    , aaName :: String
    }
  deriving (Eq, Ord, Show)


data ABlock
  = ABlock [AStatement]
  deriving (Eq, Ord, Show)


data AStatement
  = AReturn AExpr
  | APrint AExpr
  deriving (Eq, Ord, Show)


data ABinary
  = AAdd
  | ASub
  | AMul
  | ADiv
  deriving (Eq, Ord, Show)


data AUnary
  = ANeg
  | ANot
  deriving (Eq, Ord, Show)


data AExpr
  = AUnOp
    { aeTag :: ATag
    , aeUnOp :: AUnary
    , aeArg :: AExpr
    }
  | ABinOp
    { aeTag :: ATag
    , aeBinOp :: ABinary
    , aeLeft :: AExpr
    , aeRight :: AExpr
    }
  | AVar
    { aeTag :: ATag
    , aeName :: String
    }
  deriving (Eq, Ord, Show)


data AType
  = AInt
  | ABool
  | AString
  | ATuple AType AType
  | AArray AType
  deriving (Eq, Ord, Show)