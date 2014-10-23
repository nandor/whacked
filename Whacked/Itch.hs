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
  | IBool
    { ieBool :: Bool
    }
  | IInt
    { ieInt :: Int
    }
  | IChar
    { ieChar :: Char
    }
  | IArray
    { ieType :: Type
    , ieElems :: [IExpr]
    }
  | IPair
    { ieType :: Type
    , ieLeft :: IExpr
    , ieRight :: IExpr
    }
  | IIndex
    { ieType  :: Type
    , ieArray :: IExpr
    , ieIndex :: IExpr
    }
  | IElem
    { ieType :: Type
    , iePair :: IExpr
    , ieElem :: Elem
    }
  | ICall
    { ieType :: Type
    , ieName :: String
    , ieArgs :: [IExpr]
    }
  | INull
    {
    }
  deriving ( Eq, Ord, Show )


data IInstr
  = IReturn
    { iiExpr :: IExpr
    }
  | IBinJump
    { iiWhere :: Int
    , iiWhen  :: Bool
    , iiCond  :: CondOp
    , iiLeft  :: IExpr
    , iiRight :: IExpr
    }
  | IUnJump
    { iiWhere :: Int
    , iiWhen  :: Bool
    , iiVal   :: IExpr
    }
  | IJump
    { iiWhere :: Int
    }
  | IAssVar
    { iiVar   :: String
    , iiScope :: Int
    , iiExpr  :: IExpr
    }
  | IAssArray
    { iiVar   :: String
    , iiScope :: Int
    , iiIndex :: [IExpr]
    , iiExpr  :: IExpr
    }
  | IAssPair
    { iiVar   :: String
    , iiSCope :: Int
    , iiElem  :: Elem
    , iiExpr  :: IExpr
    }

  | IRead
    { iiVar   :: String
    , iiScope :: Int
    , iiType  :: Type
    }
  | IPrint
    { iiExpr :: IExpr
    }
  | IPrintln
    { iiExpr :: IExpr
    }
  | ILabel
    { iiLabel :: Int
    }
  | IFree
    { iiExpr :: IExpr
    }
  | IExit
    { iiExpr :: IExpr
    }
  | IEnd
  deriving ( Eq, Ord )


instance Show IInstr where
  show IReturn{..}   = "IReturn   "
  show IBinJump{..}  = "IBinJump  " ++ show iiWhere
  show IUnJump{..}   = "IUnJump   " ++ show iiWhere
  show IJump{..}     = "IJump     " ++ show iiWhere
  show IAssVar{..}   = "IAssVar   " ++ iiVar
  show IAssArray{..} = "IAssArray " ++ iiVar
  show IAssPair{..}  = "IAssPair  "
  show IRead{..}     = "IRead     " ++ iiVar
  show IPrint{..}    = "IPrint    "
  show IPrintln{..}  = "IPrintln  "
  show ILabel{..}    = "ILabel    " ++ show iiLabel
  show IExit{..}     = "IExit     "
  show IEnd{..}      = "IEnd      "