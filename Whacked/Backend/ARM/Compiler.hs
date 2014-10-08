{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Compiler
  ( compile
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Whacked.Backend.ARM.ASM
import           Whacked.IMF



data Scope
  = Scope
    {
    }
  deriving (Eq, Ord, Show)


newtype Compiler a
  = Compiler
    { run :: StateT Scope (Writer [ASM]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [ASM])


compileInstr :: IInstr -> Compiler ()
compileInstr IConstInt{..} = do
  tell [ARMLDR R0 iiIntVal]
compileInstr IBinOp{..} = do
  tell [ARMAdd R0 R1 R2]
compileInstr IReturn{..} = do
  tell [ARMLDM [PC]]
compileInstr _ = do
  return ()


compileFunc :: IFunction -> Compiler ()
compileFunc IFunction{..} = do
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