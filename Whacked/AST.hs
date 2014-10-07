module Whacked.AST where



data ATag
  = ATag
    { atSource :: String
    , atLine :: Int
    , atChar :: Int
    }
  deriving (Eq, Ord, Show)


data AProgram
  = AProgram
    { apFunctions :: [AFunction]
    , apMain :: [AStatement]
    }
  deriving (Eq, Ord, Show)


data AFunction
  = AFunction
    { afArgs :: [AArg]
    , afReturn :: AType
    , afName :: String
    , afBody :: [AStatement]
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


data AStatement
  = AReturn ATag AExpr
  | APrint ATag AExpr
  deriving (Eq, Ord, Show)


data ABinary
  = AAdd
  | ASub
  | AMul
  | ADiv
  | AMod
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
