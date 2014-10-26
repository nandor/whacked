{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
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
  | SImm Int
  deriving ( Eq, Ord )


data SProgram
  = SProgram
    { spFuncs :: [SFunction]
    }
  deriving ( Eq, Ord )


data SFunction
  = SFunction
    { sfBlocks :: Map Int SBlock
    , sfArgs   :: [SVar]
    , sfName   :: String
    }
  | SFlatFunction
    { sfInstrs :: [(Int, SInstr)]
    , sfArgs   :: [SVar]
    , sfName   :: String
    }
  deriving ( Eq, Ord )



data SBlock
  = SBlock
    { sbPhis   :: [SPhi]
    , sbInstrs :: [SInstr]
    }
  deriving ( Eq, Ord )


data SPhi
  = SPhi
    { spDest  :: SVar
    , spType  :: Type
    , spMerge :: [(Int, SVar)]
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
  | SMov
    { siDest :: SVar
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
  | SString
    { siDest   :: SVar
    , siString :: String
    }
  | SNewArray
    { siDest :: SVar
    , siLength :: Int
    }
  | SWriteArray
    { siArray :: SVar
    , siIndex :: SVar
    , siExpr  :: SVar
    , siType  :: Type
    }
  | SReadArray
    { siDest  :: SVar
    , siArray :: SVar
    , siIndex :: SVar
    , siType  :: Type
    }
  | SNewPair
    { siDest :: SVar
    }
  | SWritePair
    { siElem :: Elem
    , siPair :: SVar
    , siExpr :: SVar
    }
  | SReadPair
    { siElem :: Elem
    , siDest :: SVar
    , siPair :: SVar
    }
  | SFree
    { siRef :: SVar
    }
  | SReturn
    { siArg  :: SVar
    }
  | SBinJump
    { siWhere :: Int
    , siCond  :: CondOp
    , siLeft  :: SVar
    , siRight :: SVar
    }
  | SUnJump
    { siWhere :: Int
    , siWhen  :: Bool
    , siArg   :: SVar
    }
  | SJump
    { siWhere :: Int
    }
  | SLabel
    { siWhere :: Int
    }
  deriving (Eq, Ord)


instance Show SVar where
  show (SVar i)
    = "$" ++ show i
  show (SImm i)
    = "#" ++ show i


instance Show SProgram where
  show SProgram{..}
    = concat . intersperse "\n\n" . map show $ spFuncs


instance Show SFunction where
  show SFunction{..}
    = sfName ++ "(" ++ concat (intersperse "," $ map show sfArgs) ++ ")\n"
      ++ concat (intersperse "\n" $ map showBlock . Map.toList $ sfBlocks)
    where
      showBlock (i, x)
        = printf "%4d:\n%s" i (show x)
  show SFlatFunction{..}
    = sfName ++ "(" ++ concat (intersperse "," $ map show sfArgs) ++ ")\n"
      ++ concat (intersperse "\n" $ map showInstr $ sfInstrs)
    where
      showInstr (i, x)
        = printf "%4d:   %s" i (show x)


instance Show SBlock where
  show SBlock{..}
    = (concat . intersperse "\n" . map (\x -> "      " ++ show x) $ sbPhis) ++
      "\n" ++
      (concat . intersperse "\n" . map (\x -> "      " ++ show x) $ sbInstrs)


instance Show SPhi where
  show SPhi{..}
    = show spDest ++ " <- phi(" ++
      (concat . intersperse "," $ map showArg spMerge) ++
      ")"
    where
      showArg (from, var)
        = show from ++ "->" ++ show var


instance Show SInstr where
  show SBinOp{..}
    = show siDest ++ " <- " ++ show siLeft ++ show siBinOp ++ show siRight
  show SUnOp{..}
    = show siDest ++ " <- " ++ show siUnOp ++ "(" ++ show siArg ++ ")"
  show SMov{..}
    = show siDest ++ " <- " ++ show siArg
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
  show SString{..}
    = show siDest ++ " <- " ++ show siString
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
  show SFree{..}
    = "free " ++ show siRef
  show SReturn{..}
    = "ret " ++ show siArg
  show SBinJump{..}
    = "jmpbin @" ++ show siWhere ++ ", " ++
      show siLeft ++ show siCond ++ show siRight
  show SUnJump{..}
    = "jmpun @" ++ show siWhere ++ ", " ++ show siWhen ++ "=" ++ show siArg
  show SJump{..}
    = "jmp @" ++ show siWhere


-- |Applies a function to all functions.
mapF :: (SFunction -> SFunction) -> SProgram -> SProgram
mapF f prog@SProgram{..}
  = prog{ spFuncs = map f spFuncs }


-- |Maps a function over all instructions.
mapI :: (SInstr -> SInstr) -> SFunction -> SFunction
mapI f func@SFunction{..}
  = func
    { sfBlocks
        = Map.map (\b@SBlock{..} -> b{ sbInstrs = map f sbInstrs }) sfBlocks
    }


-- |Returns true if the instruction makes an assignment to a variable.
isAssignment :: SInstr -> Bool
isAssignment SBinOp{..}      = True
isAssignment SUnOp{..}       = True
isAssignment SCall{..}       = True
isAssignment SBool{..}       = True
isAssignment SChar{..}       = True
isAssignment SInt{..}        = True
isAssignment SString{..}     = True
isAssignment SNewArray{..}   = True
isAssignment SWriteArray{..} = False
isAssignment SReadArray{..}  = True
isAssignment SNewPair{..}    = True
isAssignment SWritePair{..}  = False
isAssignment SReadPair{..}   = True
isAssignment SFree{..}       = False
isAssignment SReturn{..}     = False
isAssignment SBinJump{..}    = False
isAssignment SUnJump{..}     = False
isAssignment SJump{..}       = False


-- |Returns true if a SVar is not an immediate.
isVar :: SVar -> Bool
isVar (SVar _) = True
isVar (SImm _) = False


-- |Checks if an instruction is a call instruction.
isCall :: SInstr -> Bool
isCall SCall{..} = True
isCall _         = False


-- |Returns the target of a jump instruction.
getTarget :: SInstr -> [Int]
getTarget SBinJump{..} = [siWhere]
getTarget SUnJump{..}  = [siWhere]
getTarget SJump{..}    = [siWhere]
getTarget _            = []


-- |Returns the list of variables that are overwritten by a statement.
getKill :: SInstr -> [SVar]
getKill x
  = filter isVar $ case x of
    SBinOp{..}      -> [siDest]
    SUnOp{..}       -> [siDest]
    SMov{..}        -> [siDest]
    SCall{..}       -> siRet
    SBool{..}       -> [siDest]
    SChar{..}       -> [siDest]
    SInt{..}        -> [siDest]
    SString{..}     -> [siDest]
    SNewArray{..}   -> [siDest]
    SWriteArray{..} -> []
    SReadArray{..}  -> [siDest]
    SNewPair{..}    -> [siDest]
    SWritePair{..}  -> []
    SReadPair{..}   -> [siDest]
    SFree{..}       -> [siRef]
    SReturn{..}     -> []
    SBinJump{..}    -> []
    SUnJump{..}     -> []
    SJump{..}       -> []


-- |Returns the list of variables that are used in a statement.
getGen :: SInstr -> [SVar]
getGen x
  = filter isVar $ case x of
    SBinOp{..}      -> [siLeft, siRight]
    SUnOp{..}       -> [siArg]
    SMov{..}        -> [siArg]
    SCall{..}       -> siArgs
    SBool{..}       -> []
    SChar{..}       -> []
    SInt{..}        -> []
    SString{..}     -> []
    SNewArray{..}   -> []
    SWriteArray{..} -> [siArray, siIndex, siExpr]
    SReadArray{..}  -> [siArray, siIndex]
    SNewPair{..}    -> []
    SWritePair{..}  -> [siPair, siExpr]
    SReadPair{..}   -> [siPair]
    SFree{..}       -> [siRef]
    SReturn{..}     -> [siArg]
    SBinJump{..}    -> [siLeft, siRight]
    SUnJump{..}     -> [siArg]
    SJump{..}       -> []
