{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.ConstantMover
  ( moveConstants
  ) where

import           Control.Applicative
import           Data.Bits
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set as Set
import           Whacked.FlowGraph
import           Whacked.Scratch

import Debug.Trace


-- |Checks if the variable can be encoded in an ARM instruction.
fitsInReg :: Int -> Bool
fitsInReg x
  = ((x `shiftR` 16) .&. 0xFFFFFF00) == 0


-- |Pushes immediate constants into instructions. Due to the fact that ARM
-- instructions can encode values up to 8 bits in side, the number of required
-- registers will be reduced after doing this.
moveConstants :: SFunction -> SFunction
moveConstants func@SFunction{..}
  = func{ sfBlocks = sfBlocks' }
  where
    (_, sfBlocks')
      = Map.foldlWithKey replaceBlock (Map.empty, Map.empty) sfBlocks

    replaceBlock (vars, blocks) idx block@SBlock{..}
      = (vars', Map.insert idx block{ sbInstrs = reverse sbInstrs' } blocks)
      where
        (vars', sbInstrs') = foldl replaceInstr (vars, []) sbInstrs

    replaceInstr (vars, instrs) instr
      = case instr of
        op@SBinOp{..} ->
          (vars, op{ siRight = replaceVar siRight }:instrs)
        op@SMov{..} ->
          (vars, op{ siArg = replaceVar siArg }:instrs)
        op@SBool{..} ->
          (Map.insert siDest (SImm $ fromEnum siBool) vars, op:instrs)
        op@SInt{..} | fitsInReg siInt ->
          (Map.insert siDest (SImm $ siInt) vars, op:instrs)
        op@SWriteArray{..} ->
          (vars, op{ siIndex = replaceVar siIndex }:instrs)
        op@SReadArray{..} ->
          (vars, op{ siIndex = replaceVar siIndex }:instrs)
        op@SBinJump{..} ->
          (vars, op{ siRight = replaceVar siRight }:instrs)
        _ ->
          (vars, instr:instrs)
      where
        replaceVar var = fromMaybe var $ Map.lookup var vars
