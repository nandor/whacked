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
    , strings   :: Map String String
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
      Just (ARMLocReg reg') | reg /= reg' ->
        move reg' (ARMR reg)
      Just (ARMLocStk stk) ->
        tell [ ARMStr reg SP $ ARMI (stk * 4) ]
      _ ->
        return ()
moveLocToVar (ARMLocArgIn stk) var
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Just (ARMLocReg reg') ->
        tell [ ARMLdr reg' SP $ ARMI ((regStack + varStack + stk) * 4) ]
      Just (ARMLocStk stk') -> do
        tell [ ARMLdr R12 SP $ ARMI ((regStack + varStack + stk') * 4) ]
        tell [ ARMStr R12 SP $ ARMI (stk' * 4) ]
      Nothing ->
        return ()


-- |Copies a variable to into one of the registers that is going to be passed
-- as an argument to another function.
moveVarToLoc :: SVar -> ARMLoc -> Compiler ()
moveVarToLoc (SImm x) (ARMLocReg reg) =
  tell [ ARMLoadConst reg x ]
moveVarToLoc (SImm x) (ARMLocArgOut stk) = do
  tell [ ARMMov AAL R12 (ARMI x) ]
  tell [ ARMStr R12 SP $ ARMI (-(stk + 1) * 4) ]
moveVarToLoc var (ARMLocReg reg)
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Just (ARMLocReg reg') | reg' /= reg ->
        tell [ ARMMov AAL reg (ARMR reg') ]
      Just (ARMLocStk stk) ->
        tell [ ARMLdr reg SP $ ARMI (stk * 4) ]
      _ ->
        return ()
moveVarToLoc var (ARMLocArgOut stk)
  = get >>= \Scope{..} -> case Map.lookup var regs of
      Nothing ->
        return ()
      Just (ARMLocReg reg') ->
        tell [ ARMStr reg' SP $ ARMI (-(stk + 1) * 4) ]
      Just (ARMLocStk stk') -> do
        tell [ ARMLdr R12 SP $ ARMI (stk' * 4) ]
        tell [ ARMStr R12 SP $ ARMI (-(stk + 1) * 4) ]


-- |Returns the register mapped to a value or a default temprary register.
findReg :: SVar -> ARMReg -> Compiler ARMReg
findReg var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> return reg'
    _ -> return reg

-- |Copies the result of an operation to the stack or does nothing if the
-- variable was originally in a register.
storeReg :: SVar -> ARMReg -> Compiler ()
storeReg var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> move reg' (ARMR reg)
    ARMLocStk idx -> tell [ ARMStr reg SP $ ARMI (idx * 4) ]


-- |Copies an immediate value to a memory location.
storeImm :: SVar -> ARMImm -> ARMReg -> Compiler ()
storeImm var imm tmp
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg' -> move reg' imm
    ARMLocStk idx -> do
      move tmp imm
      tell [ ARMStr tmp SP $ ARMI (idx * 4) ]


-- |Returns a register to use as an input operand for an instruction.
fetchReg :: SVar -> ARMReg -> Compiler ARMReg
fetchReg (SImm i) reg = do
  tell [ ARMMov AAL reg (ARMI i) ]
  return reg
fetchReg var reg
  = get >>= \Scope{ regs } -> case fromJust $ Map.lookup var regs of
    ARMLocReg reg  -> return reg
    ARMLocStk idx -> do
      tell [ ARMLdr reg SP $ ARMI (idx * 4) ]
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
move reg = \case
  ARMR reg' | reg /= reg' ->
    tell [ ARMMov AAL reg (ARMR reg') ]
  ARMI imm ->
    tell [ ARMMov AAL reg (ARMI imm) ]
  _ ->
    return ()


-- |Generates ARM assembly for an instruction.
compileInstr :: SInstr -> Compiler ()
compileInstr SInt{..} = do
  dest <- findReg siDest R12
  tell [ ARMLoadConst dest siInt ]
  storeReg siDest dest
compileInstr SBool{..} = do
  dest <- findReg siDest R12
  tell [ ARMMov AAL dest (ARMI $ fromEnum siBool) ]
  storeReg siDest dest
compileInstr SChar{..} = do
  dest <- findReg siDest R12
  tell [ ARMMov AAL dest (ARMI $ ord siChar) ]
  storeReg siDest dest
compileInstr SString{..} = do
  scope@Scope{..} <- get
  let label = "msg_" ++ show (Map.size strings)
  put scope{ strings = Map.insert  label siString strings }
  dest <- findReg siDest R12
  tell [ ARMAdr dest label]
  storeReg siDest dest
compileInstr SBinOp{..} = do
  dest <- findReg siDest R12
  left <- fetchReg siLeft R11
  case siBinOp of
    Add -> do
      imm <- fetchImm siRight R12
      tell [ ARMAdd AAL dest left imm ]
      tell [ ARMB AVS $ Right "__check_overflow" ]
    Sub -> do
      imm <- fetchImm siRight R12
      tell [ ARMSub AAL dest left imm ]
      tell [ ARMB AVS $ Right "__check_overflow" ]
    Or  -> do
      imm <- fetchImm siRight R12
      tell [ ARMOrr AAL dest left imm ]
    And -> do
      imm <- fetchImm siRight R12
      tell [ ARMAnd AAL dest left imm ]
    Mul -> do
      arg <- fetchReg siRight R12
      tell [ ARMSmull AAL R11 R12 left arg ]
      tell [ ARMCmpAsr AAL R12 R11 32 ]
      tell [ ARMB ANE $ Right "__check_overflow" ]
      tell [ ARMMov AAL dest (ARMR R11) ]
    Cmp op -> do
      imm <- fetchImm siRight R12
      tell [ ARMCmp AAL left imm ]
      tell [ ARMMov (toARMCond op) dest (ARMI 1) ]
      tell [ ARMMov (toARMCond $ invertCond op) dest (ARMI 0) ]
  storeReg siDest dest
compileInstr SUnOp{..} = do
  dest <- findReg siDest R12
  arg  <- fetchImm siArg R12
  case siUnOp of
    Not -> tell [ ARMMvn AAL dest arg ]
    Neg -> do
      tell [ ARMNeg AAL dest arg ]
      tell [ ARMB AVS $ Right "__check_overflow" ]
    Ord -> move dest arg
    Chr -> move dest arg -- TODO(nl1813): Check for overflow.
compileInstr SBinJump{..} = do
  left <- fetchReg siLeft R12
  imm  <- fetchImm siRight R11
  label <- getLabel siWhere
  tell [ ARMCmp AAL left imm ]
  tell [ ARMB (toARMCond siCond) $ Left label ]
compileInstr SUnJump{..} = do
  arg <- fetchReg siArg R12
  label <- getLabel siWhere
  tell [ ARMTst AAL arg (ARMI 1) ]
  tell [ ARMB (if siWhen then ANE else AEQ) $ Left label ]
compileInstr SJump{..} = do
  label <- getLabel siWhere
  tell [ ARMB AAL $ Left label ]
compileInstr SCheckNull{..} = do
  var <- fetchReg siArg R12
  tell [ ARMTeq AAL var (ARMI 0) ]
  tell [ ARMB AEQ $ Right "__check_null" ]
compileInstr SCheckZero{..} = do
  var <- fetchReg siArg R12
  tell [ ARMTeq AAL var (ARMI 0) ]
  tell [ ARMB AEQ $ Right "__check_zero" ]
compileInstr SCheckBounds{..} = do
  index <- fetchReg siIndex R12
  tell [ ARMCmp AAL index (ARMI 0) ]
  tell [ ARMB ALT $ Right "__check_bounds" ]
  array <- fetchReg siArray R12
  tell [ ARMLdr R11 array (ARMI (-4)) ]
  tell [ ARMCmp AAL index (ARMR R11) ]
  tell [ ARMB AGE $ Right "__check_bounds" ]
compileInstr SMov{ siArg = (SImm x), ..} =
  storeImm siDest (ARMI x) R12
compileInstr SMov{ siArg = x@(SVar _), ..}
  = get >>= \Scope{..} -> case fromJust $ Map.lookup siDest regs of
    ARMLocReg reg -> do
      var <- fetchReg x reg
      storeReg siDest var
    ARMLocStk stk -> do
      var <- fetchReg x R12
      storeReg siDest var
compileInstr SLength{..} = do
  dest <- findReg siDest R12
  array <- findReg siDest R12
  tell [ ARMLdr dest array (ARMI (-4)) ]
compileInstr SReadArray{..} = do
  dest <- findReg siDest R12
  array <- fetchReg siArray R12
  case siType of
    Char -> do
      index <- fetchImm siIndex R11
      tell [ ARMLdrb dest array index ]
    Bool -> do
      index <- fetchImm siIndex R11
      tell [ ARMLdrb dest array index ]
    _ -> fetchImm siIndex R11 >>= \case
      ARMI idx -> tell [ ARMLdr dest array (ARMI (idx * 4)) ]
      ARMR reg -> tell [ ARMLdrLsl dest array reg 2 ]
  storeReg siDest dest
compileInstr SWriteArray{..} = do
  array <- fetchReg siArray R12
  expr <- fetchReg siExpr R11
  case siType of
    Char -> do
      index <- fetchImm siIndex R11
      tell [ ARMStrb expr array index]
    Bool -> do
      index <- fetchImm siIndex R11
      tell [ ARMStrb expr array index]
    _ -> fetchImm siIndex R11 >>= \case
      ARMI idx ->
        tell [ ARMStr expr array (ARMI (idx * 4)) ]
      ARMR reg ->
        tell [ ARMStrLsl expr array reg 2 ]
compileInstr SWritePair{..} = do
  pair <- fetchReg siPair R12
  elem <- fetchReg siExpr R11
  tell [ ARMStr elem pair (ARMI $ if siElem == Fst then 0 else 4) ]
compileInstr SReadPair{..} = do
  dest <- findReg siDest R12
  pair <- fetchReg siPair R12
  tell [ ARMLdr dest pair (ARMI $ if siElem == Fst then 0 else 4) ]
  storeReg siDest dest
compileInstr SCall{..} = do
  -- Copy arguments to registers & stack.
  forM_ (zip siArgs (argOutLocation $ length siArgs)) $
    uncurry moveVarToLoc

  -- Call the function.
  when (length siArgs > 4) $
    tell [ARMSub AAL SP SP (ARMI $ (length siArgs - 4) * 4)]
  tell [ ARMBL siFunc]
  when (length siArgs > 4) $
    tell [ARMAdd AAL SP SP (ARMI $ (length siArgs - 4) * 4)]

  -- Move returns values to registers / stack.
  forM_ (zip siRet [ARMLocReg x | x <- enumFromTo R0 R3]) $
    uncurry (flip moveLocToVar)

compileInstr SReturn{..} = do
  arg <- fetchImm siArg R0
  Scope{ toSave, varStack } <- get
  when (varStack /= 0) $
    tell [ ARMAdd AAL SP SP (ARMI $ varStack * 4) ]
  move R0 arg
  tell [ ARMPOP (Set.toList . Set.fromList $ PC : toSave) ]


-- |Generates code for a function.
compileFunc :: SFunction -> Compiler ()
compileFunc func@SFlatFunction{..} = do
  let live = map (\(_, (x, y)) -> (Set.toList x, Set.toList y))
           . Map.toList
           $ liveVariables func
      targets  = Set.fromList $ concatMap (getTarget . snd) sfInstrs
      regPref  = getPreferredRegs live func
      regAlloc = allocRegs live func regPref

      usedRegs
        = [ x
          | ARMLocReg x <- nub $ map snd (Map.toList regAlloc)
          , x `notElem` enumFromTo R0 R3
          ]
      stackSpace
        = length . nub $ [ x | (reg,  ARMLocStk x) <- Map.toList regAlloc ]

  get >>= \scope@Scope{..} -> put scope
    { toSave = usedRegs
    , regs = regAlloc
    , regStack = 1 + length usedRegs
    , argStack = max 0 (length sfArgs - 4)
    , varStack = stackSpace
    }

  -- Emit the function header.
  tell [ ARMFunc sfName ]

  -- Push on the stack all modified registers.
  save <- toSave <$> get
  tell [ ARMPUSH (Set.toList . Set.fromList $ LR : save) ]
  when (stackSpace /= 0) $
    tell [ ARMSub AAL SP SP (ARMI $ stackSpace * 4) ]

  -- Copy arguments to their proper location.
  forM_ (zip (argInLocation $ length sfArgs) sfArgs) $
    uncurry moveLocToVar

  -- Emit instructions.
  forM_ (zip live sfInstrs) $ \((_, out), (i, instr)) -> do
    when (i `Set.member` targets) $ do
      label <- getLabel i
      tell [ ARMLabel label ]

    case getKill instr of
      [x] | not (isCall instr) && x `notElem` out -> return ()
      _ -> compileInstr instr

  tell [ ARMLtorg ]

  -- Update the label counter.
  get >>= \scope@Scope{ labelBase } ->
    put scope{ labelBase = labelBase + length sfInstrs}


compileProg :: SProgram -> Compiler ()
compileProg SProgram{..}
  = forM_ spFuncs $ \case
    func@SFlatFunction{..} -> compileFunc func
    core@SCoreFunction{..} -> tell [ARMCore sfCore]


compile :: SProgram -> [ASM]
compile program
  = [ARMSection ".data"] ++
    dataSeg ++
    [ARMSection ".text"] ++
    codeSeg
  where
    ((_, Scope{..}), codeSeg)
      = runWriter . runStateT (run $ compileProg program) $ scope

    dataSeg
      = map snd
      . Map.toList
      . Map.mapWithKey ARMString
      $ strings

    scope
      = Scope
        { regs      = Map.empty
        , toSave    = []
        , labelBase = 0
        , regStack  = 0
        , argStack  = 0
        , varStack  = 0
        , strings   = Map.empty
        }
