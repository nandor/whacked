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


-- |Copies an argument to its proper location. Does this using a minimal
-- number of instructions.
moveLocToVar :: ARMLoc -> SVar -> Compiler ()
moveLocToVar (ARMLocReg reg) var
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Nothing ->
        return ()
      Just (ARMLocReg reg') -> do
        when (reg' /= reg) $
          tell [ ARMMov reg' (ARMR reg) ]
      Just (ARMLocStk stk) -> do
        tell [ ARMStoreMem reg SP $ -stk * 4 ]
moveLocToVar (ARMLocArgIn stk) var
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Nothing ->
        return ()
      Just (ARMLocReg reg') -> do
        tell [ ARMLoadMem reg' SP $ -(regStack + varStack + stk) * 4 ]
      Just (ARMLocStk stk') -> do
        tell [ ARMLoadMem R12 SP $ -(regStack + varStack + stk) * 4 ]
        tell [ ARMStoreMem R12 SP $ -stk' * 4 ]

-- |Copies a variable to into one of the registers that is going to be passed
-- as an argument to another function.
moveVarToLoc :: SVar -> ARMLoc -> Compiler ()
moveVarToLoc var (ARMLocReg reg)
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Nothing ->
        return ()
      Just (ARMLocReg reg') -> do
        when (reg' /= reg) $
          tell [ ARMMov reg (ARMR reg') ]
      Just (ARMLocStk stk) -> do
          tell [ ARMLoadMem reg SP $ -stk * 4 ]
moveVarToLoc var (ARMLocArgOut stk)
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Nothing ->
        return ()
      Just (ARMLocReg reg') -> do
        tell [ ARMStoreMem reg' SP $ (stk + 1) * 4 ]
      Just (ARMLocStk stk') -> do
        tell [ ARMLoadMem R12 SP $ -stk' * 4 ]
        tell [ ARMStoreMem R12 SP $ (stk + 1) * 4 ]


-- |Copies the result of an operation to the stack or does nothing if the
-- variable was originally in a register.
storeReg :: SVar -> ARMReg -> Compiler ()
storeReg var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> move reg' (ARMR reg)
    ARMLocStk idx -> tell [ ARMStoreMem reg SP (-idx * 4) ]


-- |Copies an immediate value to a memory location.
storeImm :: SVar -> ARMImm -> ARMReg -> Compiler ()
storeImm var imm tmp
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> move reg' imm
    ARMLocStk idx -> do
      move tmp imm
      tell [ ARMStoreMem tmp SP (-idx * 4) ]


-- |Returns a register to use as an input operand for an instruction.
fetchReg :: SVar -> ARMReg -> Compiler ARMReg
fetchReg (SImm i) reg = do
  tell [ ARMMov reg (ARMI i) ]
  return reg
fetchReg var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg  -> return reg
    ARMLocStk idx -> do
      tell [ ARMLoadMem reg SP (-idx * 4) ]
      return reg


-- |Returns either a constant immediate or a register to use as the second
-- operand of an instruction.
fetchImm :: SVar -> ARMReg -> Compiler ARMImm
fetchImm (SImm i) reg
  = return $ ARMI i
fetchImm var reg
  = ARMR <$> fetchReg var reg


-- |Moves an immediate into a register.
move :: ARMReg -> ARMImm -> Compiler ()
move reg (ARMR reg') = do
  when (reg' /= reg) $
    tell [ ARMMov reg (ARMR reg') ]
move reg imm = do
  tell [ ARMMov reg imm ]

{-

-- |Returns the location of a variable or a temp register if it is on the stack.






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
compileInstr SInt{..} = do
  dest <- fetchReg siDest R12
  if fitsInImm siInt
    then tell [ ARMMov dest (ARMI siInt) ]
    else tell [ ARMLoadConst dest siInt ]
  storeReg siDest dest
compileInstr SBool{..} = do
  dest <- fetchReg siDest R12
  tell [ ARMMov dest (ARMI $ fromEnum siBool) ]
  storeReg siDest dest
compileInstr SChar{..} = do
  dest <- fetchReg siDest R12
  tell [ ARMMov dest (ARMI $ ord siChar) ]
  storeReg siDest dest
compileInstr SBinOp{..} = do
  dest <- fetchReg siDest R12
  left <- fetchReg siLeft R11
  case siBinOp of
    Add -> do
      imm <- fetchImm siRight R10
      tell [ ARMAdd dest left imm ]
    Sub -> do
      imm <- fetchImm siRight R10
      tell [ ARMSub dest left imm ]
  storeReg siDest dest
compileInstr SBinJump{..} = do
  left <- fetchReg siLeft R12
  imm  <- fetchImm siRight R11
  label <- getLabel siWhere
  tell [ ARMCmp left imm ]
  tell [ ARMB (toARMCond siCond) label ]
compileInstr SUnJump{..} = do
  arg <- fetchReg siArg R12
  label <- getLabel siWhere
  tell [ ARMTst arg (ARMR arg) ]
  tell [ ARMB ANE label ]
compileInstr SMov{ siArg = (SImm x), ..} = do
  storeImm siDest (ARMI x) R12
compileInstr SMov{ siArg = x@(SVar _), ..} = do
  Scope{..} <- get
  case fromJust $ Map.lookup siDest regs of
    ARMLocReg reg -> do
      var <- fetchReg x reg
      storeReg siDest var
    ARMLocStk stk -> do
      var <- fetchReg x R12
      storeReg siDest var
compileInstr SCall{..} = do
  -- Copy arguments to registers & stack.
  forM_ (zip siArgs (argOutLocation $ length siArgs)) $ \(from, to) -> do
    moveVarToLoc from to

  -- Call the function.
  when (length siArgs > 4) $ do
    tell [ARMSub SP SP (ARMI $ (length siArgs - 4) * 4)]
  tell [ ARMBL siFunc]
  when (length siArgs > 4) $ do
    tell [ARMAdd SP SP (ARMI $ (length siArgs - 4) * 4)]

  -- Move returns values to registers / stack.
  forM_ (zip siRet [ARMLocReg x | x <- enumFromTo R0 R3]) $ \(to, from) -> do
    moveLocToVar from to
compileInstr SReturn{..} = do
  arg <- fetchImm siArg R0
  Scope{ toSave, varStack } <- get
  when (varStack /= 0) $
    tell [ ARMAdd SP SP (ARMI $ varStack * 4) ]
  move R0 arg
  tell [ ARMPOP (Set.toList . Set.fromList $ PC : toSave) ]
compileInstr x = do
  traceShow x $ undefined

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
    , varStack = stackSpace
    }

  -- Emit the function header.
  tell [ ARMFunc sffName ]

  -- Push on the stack all modified registers.
  save <- toSave <$> get
  tell [ ARMPUSH (Set.toList . Set.fromList $ LR : save) ]
  when (stackSpace /= 0) $
    tell [ ARMSub SP SP (ARMI $ stackSpace * 4) ]

  -- Copy arguments to their proper location.
  forM_ (zip (argInLocation $ length sffArgs) sffArgs) $ \(from, to) -> do
    moveLocToVar from to

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
        }
