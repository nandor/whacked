{-# LANGUAGE RecordWildCards #-}
module Whacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.

import           Data.List
import           Data.Map(Map)
import qualified Data.Map as Map
import           Text.Printf(printf)
import           Whacked.Types



-- |Variables get unique indices inside their respective blocks. The combination
-- of a unique block index and a unique index in a block can distinguish all
-- variable versions.
data SVar
  = SVar Int
  deriving ( Eq, Ord )


data SProgram
  = SProgram
    { spFuncs :: [SFunction]
    }
  deriving ( Eq, Ord )


data SFunction
  = SFunction
    { sfBody :: [(Int, SInstr)]
    , sfArgs :: [SVar]
    , sfName :: String
    }
  deriving ( Eq, Ord )


data SInstr
  = SBinOp
    { siType  :: Type
    , siDest  :: SVar
    , siBinOp :: BinaryOp
    , siLeft  :: SVar
    , siRight :: SVar
    }
  | SUnOp
    { siType :: Type
    , siDest :: SVar
    , siUnOp :: UnaryOp
    , siArg  :: SVar
    }
  | SCall
    { siRet  :: [SVar]
    , siFunc :: String
    , siArgs :: [SVar]
    }
  | SBool
    { siDest :: SVar
    , siBool :: Bool
    }
  | SChar
    { siDest :: SVar
    , siChar :: Char
    }
  | SInt
    { siDest :: SVar
    , siInt  :: Int
    }
  | SBoolArray
    { siDest  :: SVar
    , siBools :: [Bool]
    }
  | SCharArray
    { siDest  :: SVar
    , siChars :: [Char]
    }
  | SIntArray
    { siDest :: SVar
    , siInts :: [Int]
    }
  | SNewArray
    { siDest :: SVar
    , siLength :: Int
    }
  | SWriteArray
    { siType  :: Type
    , siArray :: SVar
    , siIndex :: SVar
    , siExpr  :: SVar
    }
  | SReadArray
    { siType  :: Type
    , siDest  :: SVar
    , siArray :: SVar
    , siIndex :: SVar
    }
  | SNewPair
    { siType :: Type
    , siDest :: SVar
    }
  | SWritePair
    { siType :: Type
    , siElem :: Elem
    , siPair :: SVar
    , siExpr :: SVar
    }
  | SReadPair
    { siType :: Type
    , siElem :: Elem
    , siDest :: SVar
    , siPair :: SVar
    }
  | SPhi
    { siDest  :: SVar
    , siType  :: Type
    , siMerge :: [SVar]
    }
  | SFree
    { siType :: Type
    , siRef :: SVar
    }
  | SReturn
    { siType :: Type
    , siArg  :: SVar
    }
  | SBinJump
    { siType  :: Type
    , siWhere :: Int
    , siCond  :: CondOp
    , siLeft  :: SVar
    , siRight :: SVar
    }
  | SUnJump
    { siType  :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siArg   :: SVar
    }
  | SJump
    { siWhere :: Int
    }
  | SThrow
    { siThrow :: String
    }
  deriving (Eq, Ord)


instance Show SVar where
  show (SVar i)
    = "#" ++ show i


instance Show SProgram where
  show SProgram{..}
    = concat . intersperse "\n\n" . map show $ spFuncs


instance Show SFunction where
  show SFunction{..}
    = sfName ++ "(" ++ concat (intersperse "," $ map show sfArgs) ++ ")\n"
      ++ concat (intersperse "\n" $ map showInstr sfBody)
    where
      showInstr (i, x)
        = printf "%4d    %s" i (show x)


instance Show SInstr where
  show SBinOp{..}
    = show siDest ++ " <- " ++ show siLeft ++ show siBinOp ++ show siRight
  show SUnOp{..}
    = show siDest ++ " <- " ++ show siUnOp ++ "(" ++ show siArg ++ ")"
  show SCall{..}
    = (concat . intersperse "," $ map show siRet) ++
      " <- call " ++ siFunc ++
      "(" ++ (concat . intersperse "," $ map show siArgs) ++ ")"
  show SBool{..}
    = show siDest ++ " <- " ++ show siBool
  show SChar{..}
    = show siDest ++ " <- " ++ show siChar
  show SInt{..}
    = show siDest ++ " <- " ++ show siInt
  show SBoolArray{..}
    = show siDest ++ " <- " ++ show siBools
  show SCharArray{..}
    = show siDest ++ " <- " ++ show siChars
  show SIntArray{..}
    = show siDest ++ " <- " ++ show siInts
  show SNewArray{..}
    = show siDest ++ " <- new[" ++ show siLength ++ "]"
  show SWriteArray{..}
    = show siArray ++ "[" ++ show siIndex ++ "] <- " ++ show siExpr
  show SReadArray{..}
    = show siDest ++ " <- " ++ show siArray ++ "[" ++ show siIndex ++ "]"
  show SNewPair{..}
    = show siDest ++ " <- new{}"
  show SWritePair{..}
    = show siPair ++ "." ++ show siElem ++ " <- " ++ show siExpr
  show SReadPair{..}
    = show siDest ++ " <- " ++ show siPair ++ "." ++ show siElem
  show SPhi{..}
    = show siDest ++ " <- phi(" ++
      (concat . intersperse "," $ map show siMerge) ++
      ")"
  show SFree{..}
    = "free    " ++ show siRef
  show SReturn{..}
    = "ret     " ++ show siArg
  show SBinJump{..}
    = "jmpbin @" ++ show siWhere ++ ", " ++
      show siLeft ++ show siCond ++ show siRight
  show SUnJump{..}
    = "jmpun  @" ++ show siWhere
  show SJump{..}
    = "jmp    @" ++ show siWhere
  show SThrow{..}
    = "throw   " ++ show siThrow


-- |Returns true if the instruction makes an assignment to a variable.
isAssignment :: SInstr -> Bool
isAssignment SBinOp{..}      = True
isAssignment SUnOp{..}       = True
isAssignment SCall{..}       = True
isAssignment SBool{..}       = True
isAssignment SChar{..}       = True
isAssignment SInt{..}        = True
isAssignment SBoolArray{..}  = True
isAssignment SCharArray{..}  = True
isAssignment SIntArray{..}   = True
isAssignment SNewArray{..}   = True
isAssignment SWriteArray{..} = False
isAssignment SReadArray{..}  = True
isAssignment SNewPair{..}    = True
isAssignment SWritePair{..}  = False
isAssignment SReadPair{..}   = True
isAssignment SPhi{..}        = True
isAssignment SFree{..}       = False
isAssignment SReturn{..}     = False
isAssignment SBinJump{..}    = False
isAssignment SUnJump{..}     = False
isAssignment SJump{..}       = False
isAssignment SThrow{..}      = False


-- |Returns the list of variables that are overwritten by a statement.
getKill :: SInstr -> [SVar]
getKill SBinOp{..}      = [siDest]
getKill SUnOp{..}       = [siDest]
getKill SCall{..}       = siRet
getKill SBool{..}       = [siDest]
getKill SChar{..}       = [siDest]
getKill SInt{..}        = [siDest]
getKill SBoolArray{..}  = [siDest]
getKill SCharArray{..}  = [siDest]
getKill SIntArray{..}   = [siDest]
getKill SNewArray{..}   = [siDest]
getKill SWriteArray{..} = []
getKill SReadArray{..}  = [siDest]
getKill SNewPair{..}    = [siDest]
getKill SWritePair{..}  = []
getKill SReadPair{..}   = [siDest]
getKill SPhi{..}        = [siDest]
getKill SFree{..}       = [siRef]
getKill SReturn{..}     = []
getKill SBinJump{..}    = []
getKill SUnJump{..}     = []
getKill SJump{..}       = []
getKill SThrow{..}      = []


-- |Returns the list of variables that are used in a statement.
getGen :: SInstr -> [SVar]
getGen SBinOp{..}      = [siLeft, siRight]
getGen SUnOp{..}       = [siArg]
getGen SCall{..}       = siArgs
getGen SBool{..}       = []
getGen SChar{..}       = []
getGen SInt{..}        = []
getGen SBoolArray{..}  = []
getGen SCharArray{..}  = []
getGen SIntArray{..}   = []
getGen SNewArray{..}   = []
getGen SWriteArray{..} = [siArray, siIndex, siExpr]
getGen SReadArray{..}  = [siArray, siIndex]
getGen SNewPair{..}    = []
getGen SWritePair{..}  = [siPair, siExpr]
getGen SReadPair{..}   = [siPair]
getGen SPhi{..}        = siMerge
getGen SFree{..}       = [siRef]
getGen SReturn{..}     = [siArg]
getGen SBinJump{..}    = [siLeft, siRight]
getGen SUnJump{..}     = [siArg]
getGen SJump{..}       = []
getGen SThrow{..}      = []


-- |Returns the target point of a jump instruction.
getTarget :: SInstr -> Maybe Int
getTarget SBinJump{..} = Just siWhere
getTarget SUnJump{..}  = Just siWhere
getTarget SJump{..}    = Just siWhere
getTarget _            = Nothing
