{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
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


move :: SVar -> SVar -> Compiler ()
move a b
  | a == b = return ()
  | otherwise = do
    a' <- toReg a
    b' <- toImm b
    tell [ ARMMOV a' b' ]


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
  dest <- toReg siDest
  arg <- toImm siArg
  tell [ ARMMOV dest arg ]
compileInstr SCall{..} = do
  tell [ ARMBL siFunc]
compileInstr SReturn{..} = do
  return ()

compileFunc :: SFlatFunction -> Compiler ()
compileFunc func@SFlatFunction{..} = do
  let liveOut = map (snd . snd) . Map.toList . liveVariables $ func
      targets = Set.fromList $ concatMap (getTarget . snd) sffInstrs

  tell [ ARMFunc sffName ]
  forM_ (zip liveOut sffInstrs) $ \(out, (i, instr)) -> do
    when (i `Set.member` targets) $ do
      label <- toLabel i
      tell [ ARMLabel label ]

    case getKill instr of
      [x] | not (isCall instr) && not (x `Set.member` out) -> return ()
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
