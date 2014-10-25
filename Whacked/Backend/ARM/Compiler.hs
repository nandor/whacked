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
    { regs :: Map SVar ARMReg
    , live :: Map Int (Set SVar)
    , toSave :: [ARMReg]
    , strings :: [String]
    , labelBase :: Int
    }
  deriving (Eq, Ord, Show)


newtype Compiler a
  = Compiler
    { run :: StateT Scope (Writer [ASM]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [ASM])


toImm :: SVar -> Compiler ARMImm
toImm (SImm i)
  = return (ARMI i)
toImm var
  = return (ARMR R0)    -- TODO


toReg :: SVar -> Compiler ARMReg
toReg var
  = return R0


toLabel :: Int -> Compiler Int
toLabel i = do
  Scope{ labelBase } <- get
  return (i + labelBase)


move :: Either ARMReg SVar -> SVar -> Compiler ()
move (Right a) b = do
  a' <- toReg a
  toImm b >>= \case
    ARMI imm -> tell [ ARMMOV a' (ARMI imm) ]
    ARMR reg -> when (reg /= a') $ tell [ ARMMOV a' (ARMR reg) ]
move (Left a) b = do
  toImm b >>= \case
    ARMI imm -> tell [ ARMMOV a (ARMI imm) ]
    ARMR reg -> when (a /= reg) $ tell [ ARMMOV a (ARMR reg) ]


compileInstr :: SInstr -> Compiler ()
compileInstr SInt{..} = do
  dest <- toReg siDest
  tell [ ARMLDR dest siInt]
compileInstr SBool{..} = do
  dest <- toReg siDest
  tell [ ARMLDR dest (fromEnum siBool) ]
compileInstr SChar{..} = do
  dest <- toReg siDest
  tell [ ARMLDR dest (ord siChar) ]
compileInstr SBinOp{..} = do
  dest <- toReg siDest
  left <- toReg siLeft
  right <- toImm siRight
  case siBinOp of
    Add -> tell [ ARMADD dest left right ]
    Sub -> tell [ ARMSUB dest left right ]
compileInstr SBinJump{..} = do
  left <- toReg siLeft
  right <- toImm siRight
  label <- toLabel siWhere
  tell [ ARMCMP left right ]
  tell [ ARMB  (toARMCond siCond) label]
compileInstr SMov{..} = do
  move (Right siDest) siArg
compileInstr SCall{..} = do
  tell [ ARMBL siFunc]
compileInstr SReturn{..} = do
  move (Left R0) siArg
  save <- toSave <$> get
  tell [ ARMPOP (Set.toList . Set.fromList $ PC : save) ]
compileInstr _ = do
  return ()


compileFunc :: SFlatFunction -> Compiler ()
compileFunc func@SFlatFunction{..} = do
  let liveOut = map (Set.toList . snd . snd) . Map.toList . liveVariables $ func
      targets = Set.fromList $ concatMap (getTarget . snd) sffInstrs
      regPref = getPreferredRegs liveOut func
      regAlloc = allocRegs liveOut func regPref

  -- Emit the function header.
  traceShow (regAlloc) $ tell [ ARMFunc sffName ]
  save <- toSave <$> get
  tell [ ARMPUSH (Set.toList . Set.fromList $ LR : save) ]

  -- Emit instructions.
  forM_ (zip liveOut sffInstrs) $ \(out, (i, instr)) -> do
    when (i `Set.member` targets) $ do
      label <- toLabel i
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
        { regs = Map.empty
        , live = Map.empty
        , toSave = []
        , strings = []
        , labelBase = 0
        }
