{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Itch where

-- |Itch is the first intermediary language employed by whacked. It is converted
-- to Scratch by the code generated in the middleware. The purpose of itch is
-- to represent a program in a way to facilitate the computation of dominance
-- frontiers and the placement of phi nodes. When converting to scratch, trees
-- are flattened and all variables are removed and replaced by version numbers.
-- Type information is kept in the instructions themselved.

import Data.List
import Data.Set (Set)
import Whacked.Types



data IProgram
  = IProgram
    { ipFuncs :: [ IFunction ]
    }
  deriving ( Eq, Ord )


data IFunction
  = IFunction
    { ifName :: String
    , ifType :: Type
    , ifArgs :: [(Type, String)]
    , ifBody :: [IInstr]
    , ifVars :: Set (String, Int, Type)
    }
  deriving ( Eq, Ord )


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
    , ieFst :: IExpr
    , ieSnd :: IExpr
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
  | IRead
    { ieType :: Type
    }
  | INull
    {
    }
  deriving ( Eq, Ord )


data IInstr
  = IReturn
    { iiExpr :: IExpr
    }
  | IBinJump
    { iiWhere :: Int
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
    { iiArray :: IExpr
    , iiIndex :: IExpr
    , iiExpr  :: IExpr
    }
  | IAssPair
    { iiPair  :: IExpr
    , iiElem  :: Elem
    , iiExpr  :: IExpr
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


instance Show IProgram where
  show IProgram{..}
    = concat . intersperse "\n\n" . map show $ ipFuncs


instance Show IFunction where
  show IFunction{..}
    = ifName ++ "(" ++ concat (intersperse "," $ map showArg ifArgs) ++ ")\n"
      ++ concat (intersperse "\n" $ map show ifBody)
    where
      showArg (t, name)
        = "<" ++ name ++ ":0>"


instance Show IExpr where
  show IUnOp{..}
    = show ieUnOp ++ "(" ++ show ieArg ++ ")"
  show IBinOp{..}
    = "(" ++ show ieLeft ++ ")" ++ show ieBinOp ++ "(" ++ show ieRight ++ ")"
  show IVar{..}
    = "<" ++ ieName ++ ":" ++ show ieScope ++ ">"
  show IBool{..}
    = show ieBool
  show IInt{..}
    = show ieInt
  show IChar{..}
    = show ieChar
  show IArray{..}
    = "[" ++ concat (intersperse "," $ map show ieElems) ++ "]"
  show IPair{..}
    = "{" ++ show ieFst ++ "," ++ show ieSnd ++ "}"
  show IIndex{..}
    = show ieArray ++ "[" ++ show ieIndex ++ "]"
  show IElem{..}
    = show iePair ++ "." ++ show ieElem
  show ICall{..}
    = ieName ++ "(" ++ concat (intersperse "," $ map show ieArgs) ++ ")"
  show IRead{..}
    = "read"
  show INull{..}
    = "null"


instance Show IInstr where
  show IReturn{..}
    = "    ret      " ++ show iiExpr
  show IBinJump{..}
    = "    jmpbin  @" ++ show iiWhere ++ "," ++
      "(" ++ show iiLeft ++ ")" ++ show iiCond ++ "(" ++ show iiRight ++ ")"
  show IUnJump{..}
    = "    jmpun   @" ++ show iiWhere ++ "," ++ show iiWhen ++ "=" ++ show iiVal
  show IJump{..}
    = "    jmp     @" ++ show iiWhere
  show IAssVar{..}
    = "    <" ++ iiVar ++ ":" ++ show iiScope ++ ">=" ++ show iiExpr
  show IAssArray{..}
    = "    " ++ show iiArray ++ "[" ++ show iiIndex ++ "]=" ++ show iiExpr
  show IAssPair{..}
    = "    " ++ show iiPair ++ "." ++ show iiElem ++ "=" ++ show iiExpr
  show IPrint{..}
    = "    print    " ++ show iiExpr
  show IPrintln{..}
    = "    println  " ++ show iiExpr
  show ILabel{..}
    = " @" ++ show iiLabel ++ ":"
  show IExit{..}
    = "    exit     " ++ show iiExpr
  show IEnd{..}
    = "    end      "
  show IFree{..}
    = "    free     " ++ show iiExpr


-- |Approximates the number of temporaries used by an expression.
height :: IExpr -> Int
height IBinOp{..}
  = 1 + max (height ieLeft) (height ieRight)
height IIndex{..}
  = 1 + max (height ieArray) (height ieIndex)
height ICall{..}
  = 1 + maximum (map height ieArgs)
height IUnOp{..}
  = 0
height _
  = 1


-- |Checks if an expression is a constant of a given type.
isConst :: Type -> IExpr -> Bool
isConst Char IChar{} = True
isConst Bool IBool{} = True
isConst Int  IInt{}  = True
isConst _ _          = False

