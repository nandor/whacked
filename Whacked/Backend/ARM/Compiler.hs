{-# LANGUAGE RecordWildCards,
             NamedFieldPuns,
             GeneralizedNewtypeDeriving,
             LambdaCase #-}
module Whacked.Backend.ARM.Compiler
  ( compile
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe
import           Whacked.Backend.ARM.ASM
import           Whacked.Backend.ARM.Allocator
import           Whacked.Scratch
import           Whacked.Types

import Debug.Trace

data Scope
  = Scope
    { regs      :: Map SVar ARMLoc
    , toSave    :: [ARMReg]
    , strings   :: [String]
    , labelBase :: Int
    , regStack  :: Int
    , argStack  :: Int
    , varStack  :: Int
    , tmpStack  :: Int
    , spareRegs :: [ARMReg]
    }
  deriving (Eq, Ord, Show)


newtype Compiler a
  = Compiler
    { run :: StateT Scope (Writer [ASM]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [ASM])


-- |Returns a unique label number.
getLabel :: Int -> Compiler Int
getLabel i = do
  Scope{ labelBase } <- get
  return (i + labelBase)


-- |Returns the location of a variable.
getLoc :: SVar -> Compiler ARMLoc
getLoc var
  = get >>= \Scope{ regs } -> return (fromJust $ Map.lookup var regs)


-- |Returns the next temporary register.
getTemp :: Compiler ARMReg
getTemp = do
  scope@Scope{ spareRegs } <- get
  put scope{ spareRegs = tail spareRegs }
  return (head spareRegs)


-- |Restores temporary registers.
restore :: Compiler ()
restore = do
  scope <- get
  put scope{ spareRegs = enumFromTo R10 R12 }


-- |Moves an argument into a register/stack location.
moveArgument :: ARMLoc -> SVar -> Compiler ()
moveArgument (ARMLocReg arg) var
  = getLoc var >>= \case
    ARMLocReg reg -> do
      when (arg /= reg) $
        tell [ ARMMov reg (ARMR arg) ]
    ARMLocStk idx -> do
      tell [ ARMStoreMem arg SP idx ]
moveArgument (ARMArgIn arg) var
  = getLoc var >>= \case
    ARMLocReg reg -> do
      tell [ ARMLoadMem reg SP arg ]
    ARMLocStk idx -> do
      tell [ ARMLoadMem R12 SP arg ]
      tell [ ARMStoreMem R12 SP idx ]

{-

-- |Returns the location of a variable or a temp register if it is on the stack.

-- |Returns a register to use as an input operand for an instruction.
getReg :: SVar -> Compiler ARMReg
getReg var
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg  -> return reg
    ARMLocStk idx -> do
      tell [ ARMLoadMem R12 SP (idx * 4) ]
      return R12


-- |Returns either a constant immediate or a register to use as the second
-- operand of an instruction.
getImm :: SVar -> Compiler ARMImm
getImm (SImm i)
  = return $ ARMI i
getImm var
  = ARMR <$> getReg var


-- |Copies the result of an operation to the stack or does nothing if the
-- variable was originally in a register.
moveDest :: SVar -> ARMReg -> Compiler ()
moveDest var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> when (reg /= reg') $ tell [ ARMMov reg' (ARMR reg) ]
    ARMLocStk idx -> tell [ ARMStoreMem reg SP (idx * 4) ]


-- |Restores the set of spare registers.
restore :: Compiler ()
restore
  = get >>= \scope -> put scope{ spareRegs = [R9, R10, R11] }


-- |Emits a move instructions if the arguments are different.
move :: ARMReg -> ARMImm -> Compiler ()
move reg (ARMR reg') = do
  when (reg /= reg') $
    tell [ ARMMov reg (ARMR reg')]
move reg imm = do
  tell [ ARMMov reg imm ]
-}

-- |Generates ARM assembly for an instruction.
compileInstr :: SInstr -> Compiler ()
{-
compileInstr SInt{..} = do
  dest <- getDest siDest
  if fitsInImm siInt
    then tell [ ARMMov dest (ARMI siInt) ]
    else tell [ ARMLoadConst dest siInt ]
  moveDest siDest dest
compileInstr SBool{..} = do
  dest <- getDest siDest
  tell [ ARMMov dest (ARMI $ fromEnum siBool) ]
  moveDest siDest dest
compileInstr SChar{..} = do
  dest <- getDest siDest
  tell [ ARMMov dest (ARMI $ ord siChar) ]
  moveDest siDest dest
compileInstr SBinOp{..} = do
  dest <- getDest siDest
  left <- getReg siLeft
  case siBinOp of
    Add -> do
      imm <- getImm siRight
      tell [ ARMAdd dest left imm ]
    Sub -> do
      imm <- getImm siRight
      tell [ ARMSub dest left imm ]
  moveDest siDest dest
  restore
compileInstr SBinJump{..} = do
  left <- getReg siLeft
  imm <- getImm siRight
  label <- getLabel siWhere
  tell [ ARMCmp left imm ]
  tell [ ARMB (toARMCond siCond) label ]
  restore
compileInstr SMov{..} = do
  dest <- getDest siDest
  arg <- getImm siArg
  move dest arg
  moveDest siDest dest
  restore
compileInstr SCall{..} = do
  tell [ ARMBL siFunc]-}
compileInstr SReturn{..} = do
  save <- toSave <$> get
  tell [ ARMPOP (Set.toList . Set.fromList $ PC : save) ]
compileInstr _ = do
  return ()

-- |Generates code for a function.
compileFunc :: SFlatFunction -> Compiler ()
compileFunc func@SFlatFunction{..} = do
  let liveOut = map (Set.toList . snd . snd) . Map.toList . liveVariables $ func
      targets = Set.fromList $ concatMap (getTarget . snd) sffInstrs
      regPref = getPreferredRegs liveOut func
      regAlloc = allocRegs liveOut func regPref

      usedRegs
        = [ x
          | ARMLocReg x <- map snd (Map.toList regAlloc)
          , not (x `elem` (enumFromTo R0 R3))
          ]
      stackSpace
        = length . nub $ [ x | (reg,  ARMLocStk x) <- Map.toList regAlloc ]

  get >>= \scope@Scope{..} -> put scope
    { toSave = usedRegs
    , regs = regAlloc
    , regStack = 1 + length usedRegs
    , argStack = max 0 (length sffArgs - 4)
    , varStack = 0
    , tmpStack = 0
    }

  -- Emit the function header.
  traceShow regAlloc $ tell [ ARMFunc sffName ]

  -- Push on the stack all modified registers.
  save <- toSave <$> get
  tell [ ARMPUSH (Set.toList . Set.fromList $ LR : save) ]

  -- Emit instructions.
  forM_ (zip liveOut sffInstrs) $ \(out, (i, instr)) -> do
    when (i `Set.member` targets) $ do
      label <- getLabel i
      tell [ ARMLabel label ]

    case getKill instr of
      [x] | not (isCall instr) && not (x `elem` out) -> return ()
      _ -> compileInstr instr

  -- Update the label counter.
  get >>= \scope@Scope{ labelBase } -> do
    put scope{ labelBase = labelBase + length sffInstrs}


compileProg :: SFlatProgram -> Compiler ()
compileProg SFlatProgram{..} = do
  forM_ sfpFuncs $ \func -> do
    compileFunc func


compile :: SFlatProgram -> [ASM]
compile program
  = execWriter . evalStateT (run $ compileProg program) $ scope
  where
    scope
      = Scope
        { regs      = Map.empty
        , toSave    = []
        , strings   = []
        , labelBase = 0
        , regStack  = 0
        , argStack  = 0
        , varStack  = 0
        , tmpStack  = 0
        , spareRegs = [R9, R10, R11]
        }
