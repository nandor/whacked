module Whacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.

import Data.Map
import Whacked.Types



-- |Variables get unique indices inside their respective blocks. The combination
-- of a unique block index and a unique index in a block can distinguish all
-- variable versions.
data SVar
  = SVar Int
  | SVoid
  deriving ( Eq, Ord, Show )


data SProgram
  = SProgram
    { spFuncs :: [SFunction]
    }
  deriving ( Eq, Ord, Show )


data SFunction
  = SFunction
    { sfBody :: [(Int, SInstr)]
    , sfArgs :: [SVar]
    }
  deriving ( Eq, Ord, Show )


data SInstr
  = SBinOp
    { siType  :: Type
    , siDest  :: SVar
    , siBinOp :: BinaryOp
    , siLeft  :: SVar
    , siRight :: SVar
    }
  | SCall
    { siType :: Type
    , siDest :: SVar
    , siFunc :: String
    , siArgs :: [SVar]
    }
  | SConstInt
    { siDest   :: SVar
    , siIntVal :: Int
    }
  | SPhi
    { siDest  :: SVar
    , siType  :: Type
    , siMerge :: [SVar]
    }
  | SReturn
    { siType :: Type
    , siVal  :: SVar
    }
  | SBinJump
    { siType  :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siCond  :: CondOp
    , siLeft  :: SVar
    , siRight :: SVar
    }
  | SUnJump
    { siType :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siVal   :: SVar
    }
  | SJump
    { siWhere :: Int
    }
  deriving (Eq, Ord, Show)
