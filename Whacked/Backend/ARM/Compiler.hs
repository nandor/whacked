{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Compiler
  ( compile
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.Backend.ARM.ASM
import           Whacked.Backend.ARM.Allocator
import           Whacked.IMF


data Scope
  = Scope
    { varAlloc :: Map ITemp Storage
    }
  deriving (Eq, Ord, Show)


newtype Compiler a
  = Compiler
    { run :: StateT Scope (Writer [ASM]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [ASM])


compileInstr :: (Int, IInstr) -> Compiler ()
compileInstr (_, IConstInt{..}) = do
  tell [ARMLDR R0 iiIntVal]
compileInstr (_, IBinOp{..}) = do
  tell [ARMAdd R0 R1 R2]
compileInstr (_, IReturn{..}) = do
  tell [ARMLDM [PC]]
compileInstr _ = do
  return ()


compileFunc :: IFunction -> Compiler ()
compileFunc func@IFunction{..} = do
  get >>= \scope -> put scope{ varAlloc = allocate func }
  tell [ARMLabel ifName]
  forM_ ifInstr compileInstr


compileProg :: IProgram -> Compiler ()
compileProg IProgram{..} = do
  forM_ ipFuncs $ \func -> do
    compileFunc func


compile :: IProgram -> [ASM]
compile program
  = execWriter . evalStateT (run $ compileProg program) $ scope
  where
    scope = Scope
      { varAlloc = Map.empty
      }