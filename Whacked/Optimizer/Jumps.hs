{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.Jumps
  ( reduceJumps
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.Scratch
import           Whacked.Types



-- | Simplifies jumps. Performs jump threading, then removes jumps to
-- consecutive locations.
reduceFunc :: SFunction -> SFunction
reduceFunc SFunction{..}
  = SFunction . filter (not . isNext) $ sfBody
  where
    next = Map.fromList $ zip (map fst sfBody) (tail . map fst $ sfBody)

    isNext (node, SJump{..})
      = isNext' node siWhere
    isNext (node, SBinJump{..})
      = isNext' node siWhere
    isNext (node, SUnJump{..})
      = isNext' node siWhere
    isNext _
      = False

    isNext' node idx
      = case Map.lookup node next of
        Nothing -> False
        Just idx' -> idx == idx'

reduceJumps :: SProgram -> SProgram
reduceJumps SProgram{..}
  = SProgram $ map reduceFunc spFuncs

