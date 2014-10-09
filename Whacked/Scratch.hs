module Wahacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.


data IProgram
  = IProgram
    { ipFuncs :: [IFunction]
    }
  deriving ( Eq, Ord, Show )


data IFunction
  = IFunction
    { ifName :: String
    , ifInstr :: [IInstr]
    }
  deriving ( Eq, Ord, Show )


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
  | ILabel
    { iiIndex :: Int
    }
  | IPhi
    { iiIndex :: Int
    }
  deriving ( Eq, Ord, Show )