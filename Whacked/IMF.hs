module Whacked.IMF where


import Whacked.Ops



data IType
  = IInt
  | IBool
  deriving (Eq, Ord, Show)


data ICond
  = ILT
  | IGT
  | ILTE
  | IGTE
  | IEQ
  | INEQ
  deriving (Eq, Ord, Show)


type ITemp = Int


data IProgram
  = IProgram
    { ipFuncs :: [IFunction]
    }
  deriving (Eq, Ord, Show)


data IFunction
  = IFunction
    { ifName :: String
    , ifInstr :: [(Int, IInstr)]
    }
  deriving (Eq, Ord, Show)


data IInstr
  = ICall
    { iiReturn :: Maybe (IType, ITemp)
    , iiFunc :: String
    , iiArgs :: [ITemp]
    }
  | IArg
    { iiType :: IType
    , iiDest :: ITemp
    }
  | IReturn
    { iiType :: IType
    , iiDest :: ITemp
    }
  | IConstInt
    { iiDest :: ITemp
    , iiIntVal :: Int
    }
  | IBinOp
    { iiType :: IType
    , iiBinOp :: BinaryOp
    , iiDest :: ITemp
    , iiLeft :: ITemp
    , iiRight :: ITemp
    }
  | IBinJump
    { iiWhere :: Int
    , iiCond :: ICond
    , iiLeft :: ITemp
    , iiRight :: ITemp
    }
  deriving (Eq, Ord, Show)