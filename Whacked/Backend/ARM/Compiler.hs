{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Compiler
  ( compile
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
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
import           Whacked.LiveVariables


import Debug.Trace


data Scope
  = Scope
    { regs :: Map SVar ARMReg
    , live :: Map Int (Set SVar)
    , toSave :: [ARMReg]
    }
  deriving (Eq, Ord, Show)


newtype Compiler a
  = Compiler
    { run :: StateT Scope (Writer [ASM]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [ASM])


move :: ARMReg -> ARMReg -> Compiler ()
move to from
  | to == from = return ()
  | otherwise = tell [ARMMov to from]


compileInstr :: (Int, SInstr) -> Compiler ()
compileInstr (_, SBinOp{..}) = do
  scope@Scope{ regs } <- get
  tell $ case siBinOp of
    Add ->
      [ ARMAdd
          (fromJust $ Map.lookup siDest regs)
          (fromJust $ Map.lookup siLeft regs)
          (fromJust $ Map.lookup siRight regs)
      ]
compileInstr (_, SCall{..}) = do
  scope@Scope{ regs } <- get

  -- Move arguments to r0 - r3, place rest on stack.
  forM_ (zip siArgs (enumFrom R0)) $ \(arg, reg) -> do
    move reg (fromJust $ Map.lookup arg regs)

  tell [ARMBL siFunc]

  -- Copy return value to an appropiate register.
  case siType of
    Void -> return ()
    Int -> move (fromJust $ Map.lookup siDest regs) R0

compileInstr (_, SConstInt{..}) = do
  scope@Scope{ regs } <- get
  tell [ARMLDR (fromJust $ Map.lookup siDest regs) siIntVal]
compileInstr (_, SReturn{..}) = do
  scope@Scope{ regs, toSave } <- get
  move R0 (fromJust $ Map.lookup siVal regs)
  tell [ARMPop $ toSave ++ [PC]]
compileInstr (_, SBinJump{..}) = do
  scope@Scope{ regs } <- get
  tell [ ARMCmp
        (fromJust $ Map.lookup siLeft regs)
        (fromJust $ Map.lookup siRight regs)
       ]
  case siCond of
    CLT -> tell [ARMB ALT $ "L" ++ show siWhere]
    CGT -> tell [ARMB AGT $ "L" ++ show siWhere]
compileInstr (_, SUnJump{..})
  = return ()
compileInstr (_, SJump{..}) = do
  tell [ARMB AAL $ "L" ++ show siWhere]


compileFunc :: SFunction -> Compiler ()
compileFunc func@SFunction{..} = do
  let regs = allocate live func
      live = liveVariables func
      live' = Map.map snd live
      toSave = (nub . sort . map snd . Map.toList $ regs) \\ enumFromTo R0 R3
      target
        = Set.fromList
        . map fromJust
        . filter isJust
        . map (getTarget . snd)
        $ sfBody
  put Scope{ regs = regs, live = live', toSave }

  tell [ARMLabel sfName]
  tell [ARMPush $ toSave ++ [LR]]
  forM_ (zip sfArgs (enumFrom R0)) $ \(arg, reg) -> do
    case Map.lookup arg regs of
      Nothing -> return ()
      Just arg' -> move arg' reg
  forM_ sfBody $ \(i, instr) -> do

    -- Place a label if anything jumps here.
    when (i `Set.member` target) $ do
      tell [ARMLabel $ "L" ++ show i]

    case getKill instr of
      [x] | not $ isCall instr -> do
        when (Set.member x (fromJust $ Map.lookup i live')) $ do
          compileInstr (i, instr)
      _ ->
        compileInstr (i, instr)



compileProg :: SProgram -> Compiler ()
compileProg SProgram{..} = do
  forM_ spFuncs $ \func -> do
    compileFunc func


compile :: SProgram -> [ASM]
compile program
  = execWriter . evalStateT (run $ compileProg program) $ scope
  where
    scope
      = Scope
        { regs = Map.empty
        , live = Map.empty
        , toSave = []
        }
