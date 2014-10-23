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
  | SConstBool
    { siDest    :: SVar
    , siBoolVal :: Bool
    }
  | SConstChar
    { siDest    :: SVar
    , siCharVal :: Char
    }
  | SConstInt
    { siDest   :: SVar
    , siIntVal :: Int
    }
  | SConstString
    { siDest      :: SVar
    , siStringVal :: String
    }
  | SWriteArray
    { siType  :: Type
    , siDest  :: SVar
    , siArg   :: SVar
    , siIndex :: SVar
    , siExpr  :: SVar
    }
  | SPhi
    { siDest  :: SVar
    , siType  :: Type
    , siMerge :: [SVar]
    }
  | SReturn
    { siType :: Type
    , siArg  :: SVar
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
    { siType  :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siArg   :: SVar
    }
  | SJump
    { siWhere :: Int
    }
  | SPrint
    { siType :: Type
    , siArg  :: SVar
    }
  deriving (Eq, Ord)

instance Show SVar where
  show (SVar i)
    = "@" ++ show i


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
      " <- call " ++
      (concat . intersperse "," $ map show siArgs)
  show SConstBool{..}
    = show siDest ++ " <- " ++ show siBoolVal
  show SConstChar{..}
    = show siDest ++ " <- " ++ show siCharVal
  show SConstInt{..}
    = show siDest ++ " <- " ++ show siIntVal
  show SConstString{..}
    = ""
  show SWriteArray{..}
    = ""
  show SPhi{..}
    = ""
  show SReturn{..}
    = "ret    " ++ show siArg
  show SBinJump{..}
    = "jmpbin " ++ show siWhere
  show SUnJump{..}
    = "jmpun  " ++ show siWhere
  show SJump{..}
    = "jmp    " ++ show siWhere
  show SPrint{..}
    = "print  " ++ show siArg


isAssignment :: SInstr -> Bool
isAssignment SBinOp{} = True
isAssignment SCall{} = True
isAssignment SConstInt{} = True
isAssignment SPhi{} = True
isAssignment _ = False


getKill :: SInstr -> [SVar]
getKill SCall{..}
  = siRet
getKill SBinOp{..}
  = [siDest]
getKill SConstInt{..}
  = [siDest]
getKill SConstString{..}
  = [siDest]
getKill SConstBool{..}
  = [siDest]
getKill SConstChar{..}
  = [siDest]
getKill SPhi{..}
  = [siDest]
getKill SUnOp{..}
  = [siDest]
getKill SWriteArray{..}
  = [siDest]
getKill _
  = []


getGen :: SInstr -> [SVar]
getGen SBinOp{..}
  = [siLeft, siRight]
getGen SCall{..}
  = siArgs
getGen SPhi{..}
  = siMerge
getGen SReturn{..}
  = [siArg]
getGen SBinJump{..}
  = [siLeft, siRight]
getGen SUnJump{..}
  = [siArg]
getGen SUnOp{..}
  = [siArg]
getGen SPrint{..}
  = [siArg]
getGen SWriteArray{..}
  = [siArg, siIndex, siExpr]
getGen _
  = []

getTarget :: SInstr -> Maybe Int
getTarget SBinJump{..}
  = Just siWhere
getTarget SUnJump{..}
  = Just siWhere
getTarget SJump{..}
  = Just siWhere
getTarget _
  = Nothing


isCall :: SInstr -> Bool
isCall SCall{}
  = True
isCall _
  = False