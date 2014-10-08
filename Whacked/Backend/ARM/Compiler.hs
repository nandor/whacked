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


compileFunction :: IFunction -> Compiler ()
compileFunction IFunction{..} = do
  tell [ARMLabel ifName]
  return ()


compileProgram :: IProgram -> Compiler ()
compileProgram IProgram{..} = do
  forM_ ipFuncs $ \func -> do
    compileFunction func


compile :: IProgram -> [ASM]
compile program
  = execWriter . evalStateT (run $ compileProgram program) $ scope
  where
    scope = Scope