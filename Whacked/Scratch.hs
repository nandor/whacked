{-# LANGUAGE RecordWildCards, NamedFieldPuns, LambdaCase #-}
module Whacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.

import           Data.List
import           Data.Map(Map)
import qualified Data.Map as Map
import           Text.Printf(printf)
import           Whacked.Types


-- |List of builtin functions.
data SCore
  = SReadInt
  | SReadChar
  | SPrintInt
  | SPrintChar
  | SPrintBool
  | SPrintString
  | SPrintRef
  | SAlloc
  | SDelete
  | SThrow String
  deriving ( Eq, Ord, Show )


-- |List of core functions for which the backend must provide an implementation
-- in assembly language.
coreFunctions :: [SFunction]
coreFunctions
  = [ SCoreFunction "__read_int"       SReadInt
    , SCoreFunction "__read_char"      SReadChar
    , SCoreFunction "__print_int"      SPrintInt
    , SCoreFunction "__print_char"     SPrintChar
    , SCoreFunction "__print_bool"     SPrintBool
    , SCoreFunction "__print_string"   SPrintString
    , SCoreFunction "__print_ref"      SPrintRef
    , SCoreFunction "__alloc"          SAlloc
    , SCoreFunction "__delete"         SDelete
    , SCoreFunction "__check_null"     $ SThrow "Null pointer dereference."
    , SCoreFunction "__check_overflow" $ SThrow "Integer overflow."
    , SCoreFunction "__check_range"    $ SThrow "Index out of range."
    ]


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
  | SCoreFunction
    { sfName :: String
    , sfCore :: SCore
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
    { siType   :: Type
    , siDest   :: SVar
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
    { siDest :: SVar
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
  | SCheckNull
    { siArg :: SVar
    }
  deriving (Eq, Ord)


instance Show SVar where
  show (SVar i)
    = "$" ++ show i
  show (SImm i)
    = "#" ++ show i


instance Show SProgram where
  show SProgram{..}
    = intercalate "\n\n" . map show $ spFuncs


instance Show SFunction where
  show SFunction{..}
    = sfName ++ "(" ++ intercalate "," (map show sfArgs) ++ ")\n"
      ++ intercalate "\n" (map showBlock . Map.toList $ sfBlocks)
    where
      showBlock (i, x)
        = printf "%4d:\n%s" i (show x)
  show SFlatFunction{..}
    = sfName ++ "(" ++ intercalate "," (map show sfArgs) ++ ")\n"
      ++ intercalate "\n" (map showInstr sfInstrs)
    where
      showInstr (i, x)
        = printf "%4d:   %s" i (show x)
  show SCoreFunction{..}
    = sfName ++ "(..) CORE \n"


instance Show SBlock where
  show SBlock{..}
    = intercalate "\n" (map (\x -> "      " ++ show x) sbPhis) ++
      "\n" ++
      intercalate "\n" (map (\x -> "      " ++ show x) sbInstrs)


instance Show SPhi where
  show SPhi{..}
    = show spDest ++ " <- phi(" ++
      intercalate "," (map showArg spMerge) ++
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
    = intercalate "," (map show siRet) ++
      " <- call " ++ siFunc ++
      "(" ++ intercalate "," (map show siArgs) ++ ")"
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
    = "free " ++ show siDest
  show SReturn{..}
    = "ret " ++ show siArg
  show SBinJump{..}
    = "jmpbin @" ++ show siWhere ++ ", " ++
      show siLeft ++ show siCond ++ show siRight
  show SUnJump{..}
    = "jmpun @" ++ show siWhere ++ ", " ++ show siWhen ++ "=" ++ show siArg
  show SJump{..}
    = "jmp @" ++ show siWhere
  show SCheckNull{..}
    = "nullchk " ++ show siArg


-- |Applies a function to all functions.
mapF :: (SFunction -> SFunction) -> SProgram -> SProgram
mapF f prog@SProgram{..}
  = prog{ spFuncs = map transform spFuncs }
  where
    transform func@SFunction{}
      = f func
    transform func@SFlatFunction{}
      = f func
    transform x
      = x


-- |Maps a function over all instructions.
concatMapI :: (SInstr -> [SInstr]) -> SFunction -> SFunction
concatMapI f func@SFunction{..}
  = func
    { sfBlocks
        = Map.map (\b -> b{ sbInstrs = concatMap f (sbInstrs b)}) sfBlocks
    }


-- |Returns true if the instruction makes an assignment to a variable.
isAssignment :: SInstr -> Bool
isAssignment = \case
  SBinOp{..}      -> True
  SUnOp{..}       -> True
  SCall{..}       -> True
  SBool{..}       -> True
  SChar{..}       -> True
  SInt{..}        -> True
  SString{..}     -> True
  SNewArray{..}   -> True
  SWriteArray{..} -> False
  SReadArray{..}  -> True
  SNewPair{..}    -> True
  SWritePair{..}  -> False
  SReadPair{..}   -> True
  SFree{..}       -> False
  SReturn{..}     -> False
  SBinJump{..}    -> False
  SUnJump{..}     -> False
  SJump{..}       -> False


-- |Returns true if an expression is a terminal in the call graph.
isTerminal :: SInstr -> Bool
isTerminal = \case
  SCall{..}       -> True
  SReturn{..}     -> True
  _               -> False

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
    SFree{..}       -> [siDest]
    SReturn{..}     -> []
    SBinJump{..}    -> []
    SUnJump{..}     -> []
    SJump{..}       -> []
    SCheckNull{..}  -> []


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
    SFree{..}       -> [siDest]
    SReturn{..}     -> [siArg]
    SBinJump{..}    -> [siLeft, siRight]
    SUnJump{..}     -> [siArg]
    SJump{..}       -> []
    SCheckNull{..}  -> [siArg]