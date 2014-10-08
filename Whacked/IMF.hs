module Whacked.IMF where


import Whacked.Ops



data IType
  = IInt
  | IBool
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
    , ifInstr :: [IInstr]
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
  | IMov
    { iiType :: IType
    , iiDest :: ITemp
    , iiSource :: ITemp
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
  deriving (Eq, Ord, Show)