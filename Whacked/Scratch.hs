module Whacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.



data SProgram
  = SProgram
    { spFuncs :: [SFunction]
    }
  deriving ( Eq, Ord, Show )


data SFunction
  = SFunction
  deriving ( Eq, Ord, Show )

{-

data SInstr
  = SCall
    { iiReturn :: Maybe (IType, STemp)
    , iiFunc :: String
    , iiArgs :: [ITemp]
    }
  | SArg
    { iiType :: SType
    , iiDest :: STemp
    }
  | SReturn
    { iiType :: SType
    , iiDest :: STemp
    }
  | SConstInt
    { iiDest :: STemp
    , iiIntVal :: Snt
    }
  | SBinOp
    { iiType :: SType
    , iiBinOp :: BinaryOp
    , iiDest :: STemp
    , iiLeft :: STemp
    , iiRight :: STemp
    }
  | SBinJump
    { iiWhere :: Snt
    , iiCond :: SCond
    , iiLeft :: STemp
    , iiRight :: STemp
    }
  | SLabel
    { iiIndex :: Snt
    }
  | SPhi
    { iiIndex :: Snt
    }
  deriving ( Eq, Ord, Show )-}