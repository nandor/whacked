module Whacked.IMF where


import Whacked.Ops



data IType
  = IInt
  | IBool
  deriving (Eq, Ord, Show)

type ITemp = Int


data IProgram
  = IProgram [IFunction]
  deriving (Eq, Ord, Show)


data IFunction
  = IFunction
    { ifName :: String
    , ifInstr :: [IInstr]
    }
  deriving (Eq, Ord, Show)


data IInstr
  = ICall
    { iiType :: IType
    , iiDest :: ITemp
    , iiFunc :: String
    , iiArgs :: [ITemp]
    }
  | IArg
    { iiType :: IType
    , iiDest :: ITemp
    }
  | IPrint
    { iiType :: IType
    , iiPrint :: ITemp
    }
  | IRet
    { iiType :: IType
    , iiPrint :: ITemp
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