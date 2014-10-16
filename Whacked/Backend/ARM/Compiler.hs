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
import           Whacked.Scratch
import           Whacked.Types


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


compileInstr :: (Int, SInstr) -> Compiler ()
compileInstr instr = return ()


compileFunc :: SFunction -> Compiler ()
compileFunc func@SFunction{..} = do
  tell [ARMLabel sfName]
  forM_ sfBody compileInstr


compileProg :: SProgram -> Compiler ()
compileProg SProgram{..} = do
  forM_ spFuncs $ \func -> do
    compileFunc func


compile :: SProgram -> [ASM]
compile program
  = execWriter . evalStateT (run $ compileProg program) $ scope
  where
    scope = Scope
      {
      }