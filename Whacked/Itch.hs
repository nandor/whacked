{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Itch where

-- |Itch is the first intermediary language employed by whacked. It is converted
-- to Scratch by the code generated in the middleware. The purpose of itch is
-- to represent a program in a way to facilitate the computation of dominance
-- frontiers and the placement of phi nodes. When converting to scratch, trees
-- are flattened and all variables are removed and replaced by version numbers.
-- Type information is kept in the instructions themselved.

import Data.Set (Set)
import Whacked.Types



data IProgram
  = IProgram
    { ipFuncs :: [ IFunction ]
    }
  deriving ( Eq, Ord, Show )


data IFunction
  = IFunction
    { ifName :: String
    , ifType :: Type
    , ifArgs :: [(Type, String)]
    , ifBody :: [IInstr]
    , ifVars :: Set (String, Int, Type)
    }
  deriving ( Eq, Ord, Show )


data IExpr
  = IUnOp
    { ieType :: Type
    , ieUnOp :: UnaryOp
    , ieArg  :: IExpr
    }
  | IBinOp
    { ieType  :: Type
    , ieBinOp :: BinaryOp
    , ieLeft  :: IExpr
    , ieRight :: IExpr
    }
  | IVar
    { ieType  :: Type
    , ieName  :: String
    , ieScope :: Int
    }
  | IConstInt
    { ieIntVal :: Int
    }
  | IConstReal
    { ieRealVal :: Float
    }
  | IConstChar
    { ieCharVal :: Char
    }
  | IConstString
    { ieStringVal :: String
    }
  | ICall
    { ieType :: Type
    , ieName :: String
    , ieArgs :: [IExpr]
    }
  deriving (Eq, Ord, Show)


data IInstr
  = IReturn
    { iiExpr :: IExpr
    }
  | ICJump
    { iiWhere :: Int
    , iiWhen  :: Bool
    , iiCond  :: IExpr
    }
  | IUJump
    { iiWhere :: Int
    }
  | IWriteVar
    { iiVar   :: String
    , iiScope :: Int
    , iiExpr  :: IExpr
    }
  | IPrint
    { iiExpr :: IExpr
    }
  | ILabel
    { iiIndex :: Int
    }
  deriving ( Eq, Ord )

instance Show IInstr where
  show IReturn{..} = "IReturn"
  show ICJump{..} = "ICJump " ++ show iiWhere
  show IUJump{..} = "IUJump " ++ show iiWhere
  show IWriteVar{..} = "IWriteVar" ++ show iiVar
  show IPrint{..} = "IPrint"
  show ILabel{..} = "ILabel " ++ show iiIndex
