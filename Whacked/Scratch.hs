{-# LANGUAGE RecordWildCards #-}
module Whacked.Scratch where

-- |The last intermediary form. Optimizations such as inlining, constant
-- folding, dead code elimination and sparse conditional constant propagation
-- are applied on this form.

import           Data.Map(Map)
import qualified Data.Map as Map
import           Whacked.Types



-- |Variables get unique indices inside their respective blocks. The combination
-- of a unique block index and a unique index in a block can distinguish all
-- variable versions.
data SVar
  = SVar Int
  | SConst Int
  deriving ( Eq, Ord, Show )


-- |A constant cache holds arrays of characters, integers or booleans.
newtype SConstCache a
  = SConstCache { cache :: Map Int [a]}
  deriving ( Eq, Ord, Show )


-- |Programs hold a number of functions and constant caches.
data SProgram
  = SProgram
    { spFuncs     :: [SFunction]
    , sfArrayInt  :: SConstCache Int
    , sfArrayChar :: SConstCache Char
    , sfArrayBool :: SConstCache Bool
    }
  deriving ( Eq, Ord, Show )


-- |Functions hold the information about the argument list and the instructions
-- inside the function. They reference the constant cache which is attached to
-- the program.
data SFunction
  = SFunction
    { sfBody :: [(Int, SInstr)]
    , sfArgs :: [SVar]
    , sfName :: String
    }
  deriving ( Eq, Ord, Show )


-- |Instructions describe atomic operations. The instructions are designed in
-- such a way that they are easy to optimize. The translator uses most of them,
-- but optimization passes will transform the instruction sequence into more
-- primite operations. After the SCCP pass, only jumps, binary and unary
-- operations, function calls and array accesses are left.
data SInstr
  -- |Binary operation. Keeps track of type information and arguments. When
  -- code is generated, the right argument is always a SVar. Howevers, the
  -- SCCP pass might detect that it is a constant that can be encoded into
  -- the instruction, so it might transform it into a SConst.
  = SBinOp
    { siType  :: Type
    , siDest  :: SVar
    , siBinOp :: BinaryOp
    , siLeft  :: SVar
    , siRight :: SVar
    , siCheck :: Bool
    }

  -- |Unary operation. The argument start out as a SVar, but similarly to binary
  -- operations it might end up as a SConst. If the SCCP pass cannot prove
  -- that arguments are in range for a Chr operation, a runtime check is
  -- inserted to throw a runtime exception in case of an error.
  | SUnOp
    { siType  :: Type
    , siDest  :: SVar
    , siUnOp  :: UnaryOp
    , siArg   :: SVar
    , siCheck :: Bool
    }

  -- |Return instruction. When it is translated, the argument is copied into R0.
  -- The register allocator will always try to place the argument to this
  -- instruction into R0 in order to prevent an unnecessary assignment.
  | SReturn
    { siType :: Type
    , siVal  :: SVar
    }

  -- |Function call. Can handle functions with multiple return values in order
  -- to correctly translate modulus into __aeabi_idivmod and take the return
  -- value from the second argument. When it comes to register allocation,
  -- it is always assument that a function call thrashes R0-R3, so no variables
  -- that are live at that point can be placed into those register. The
  -- allocator will still try keep the return values and the arguments in R0-R3.
  | SCall
    { siRet  :: [SVar]
    , siFunc :: String
    , siArgs :: [SVar]
    }

  -- |Array modification. Generates a new version of the array every time an
  -- element is modified. Inserts a runtime check to verify that the index
  -- is inside bounds.
  | SWriteArray
    { siType  :: Type
    , siDest  :: SVar
    , siArg   :: SVar
    , siIndex :: SVar
    , siExpr  :: SVar
    , siCheck :: Bool
    }

  -- |Reads an element from the array. Inserts a runtime check to see if the
  -- index is in bounds.
  | SReadArray
    { siType  :: Type
    , siDest  :: SVar
    , siArg   :: SVar
    , siIndex :: SVar
    , siCheck :: Bool
    }

  -- |Creates a new array by cloning an existing one from the heap. Constant
  -- arrays are usually created by this. Arrays are all allocated on the heap,
  -- but if the actual reference is not required since the array is not modified
  -- or references to it are not compared, the arrays can be kept in the
  -- read only data segment.
  | SCloneArray
    { siType   :: Type
    , siDest   :: SVar
    , siLength :: Int
    , siWhat   :: Int
    }

  -- |Creates a new array by allocating storage space for it and the filling it
  -- in. Non constant arrays are allocated using this method.
  | SNewArray
    { siType :: Type
    , siDest :: SVar
    , siArgs :: [SVar]
    }

  -- |Reads a field from a pair. Inserts runtime checks to see if it is null
  -- or not.
  | SReadPair
    { siType :: Type
    , siDest :: SVar
    , }

  -- |Phi nodes used by the SCCP pass.
  | SPhi
    { siDest  :: SVar
    , siType  :: Type
    , siMerge :: [SVar]
    }

  -- |Prints a value. Translated into a function call after the SCCP pass.
  | SPrint
    { siType    :: Type
    , siArg     :: SVar
    , siNewline :: Bool
    }

  -- |Reads a value. Translated into a function call after the SCCP pass.
  | SRead
    { siType :: Type
    , siDest :: SVar
    }

  -- |Check if a binary operation is true and jumps to a target.
  | SBinJump
    { siType  :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siCond  :: CondOp
    , siLeft  :: SVar
    , siRight :: SVar
    }

  -- |Checks if a value is true and jumps to a target.
  | SUnJump
    { siType  :: Type
    , siWhere :: Int
    , siWhen  :: Bool
    , siVal   :: SVar
    }

  -- |Unconditional jump instruction.
  | SJump
    { siWhere :: Int
    }
  deriving (Eq, Ord, Show)


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
  = [siVal]
getGen SBinJump{..}
  = [siLeft, siRight]
getGen SUnJump{..}
  = [siVal]
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