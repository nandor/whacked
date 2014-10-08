module Whacked.AST where


import Whacked.Ops


data AType
  = AVoid
  | AInt
  | ABool
  | AString
  | ATuple AType AType
  | AArray AType
  deriving (Eq, Ord, Show)


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
  = AReturn
    { asTag :: ATag
    , asExpr :: AExpr
    }
  | APrint
    { asTag :: ATag
    , asExpr :: AExpr
    }
  | AAssign
    { asTag :: ATag
    , asTo :: ALValue
    , asExpr :: AExpr
    }
  | AVarDecl
    { asTag :: ATag
    , asType :: AType
    , asVars :: [(ATag, String, Maybe AExpr)]
    }
  | AWhile
    { asTag :: ATag
    , asExpr :: AExpr
    , asBody :: [AStatement]
    }
  | ABlock
    { asTag :: ATag
    , asBody :: [AStatement]
    }
  deriving (Eq, Ord, Show)


data AExpr
  = AUnOp
    { aeTag :: ATag
    , aeUnOp :: UnaryOp
    , aeArg :: AExpr
    }
  | ABinOp
    { aeTag :: ATag
    , aeBinOp :: BinaryOp
    , aeLeft :: AExpr
    , aeRight :: AExpr
    }
  | AVar
    { aeTag :: ATag
    , aeName :: String
    }
  | AConstInt
    { aeTag :: ATag
    , aeIntVal :: Int
    }
  | ACall
    { aeTag :: ATag
    , aeName :: String
    , aeArgs :: [AExpr]
    }
  deriving (Eq, Ord, Show)


data ALValue
  = ALVar
    { alTag :: ATag
    , alName :: String
    }
  deriving (Eq, Ord, Show)
