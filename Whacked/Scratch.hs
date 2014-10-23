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
  | SEmptyArray
    { siDest :: SVar
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
  show SEmptyArray{..}
    = show siDest ++ " <- []"
  show SPhi{..}
    = show siDest ++ " <- phi(" ++
      (concat . intersperse "," $ map show siMerge) ++
      ")"
  show SReturn{..}
    = "ret    " ++ show siArg
  show SBinJump{..}
    = "jmpbin @" ++ show siWhere ++ ", " ++
      show siLeft ++ show siCond ++ show siRight
  show SUnJump{..}
    = "jmpun  @" ++ show siWhere
  show SJump{..}
    = "jmp    @" ++ show siWhere


isAssignment :: SInstr -> Bool
isAssignment SBinOp{} = True
isAssignment SCall{} = True
isAssignment SInt{} = True
isAssignment SPhi{} = True
isAssignment _ = False


getKill :: SInstr -> [SVar]
{-
getKill SCall{..}  = siRet
getKill SBinOp{..} = [siDest]
getKill SInt{..}   = [siDest]
getKill SBool{..}  = [siDest]
getKill SChar{..}  = [siDest]
getKill SPhi{..}   = [siDest]
getKill SUnOp{..}  = [siDest]
getKill SWriteArray{..} = [siDest]
getKill _ = []
-}

getKill _ = undefined

getGen :: SInstr -> [SVar]

{-
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
-}

getGen _ = undefined

getTarget :: SInstr -> Maybe Int

{-
getTarget SBinJump{..}
  = Just siWhere
getTarget SUnJump{..}
  = Just siWhere
getTarget SJump{..}
  = Just siWhere
getTarget _
  = Nothing
-}

getTarget _ = undefined

isCall :: SInstr -> Bool
isCall SCall{}
  = True
isCall _
  = False