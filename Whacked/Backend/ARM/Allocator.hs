{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Allocator
  ( allocate
  , Storage
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.IMF
import           Whacked.Backend.ARM.ASM


data StackSpace
  = StackSpace
    { ssIndex :: Int
    , ssSize :: Int
    }
  deriving (Eq, Ord, Show)


data Storage
  = Either ARMReg StackSpace
  deriving (Eq, Ord, Show)


data Scope
  = Scope
    { varAlloc :: Map ITemp Storage
    }
  deriving (Eq, Ord, Show)


newtype Allocator a
  = Allocator
    { run :: State Scope a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope)


allocate :: IFunction -> Map ITemp Storage
allocate IFunction{..}
  = Map.empty