{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.Simplifier
  ( moveConstants
  , simplify
  ) where

import           Control.Applicative
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set as Set
import           Whacked.FlowGraph
import           Whacked.Scratch
import           Whacked.Types



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
        op@SInt{..} | fitsInImm siInt ->
          (Map.insert siDest (SImm $ siInt) vars, op:instrs)
        op@SWriteArray{..} ->
          (vars, op{ siIndex = replaceVar siIndex }:instrs)
        op@SReadArray{..} ->
          (vars, op{ siIndex = replaceVar siIndex }:instrs)
        op@SBinJump{..} ->
          (vars, op{ siRight = replaceVar siRight }:instrs)
        op@SReturn{..} ->
          (vars, op{ siArg = replaceVar siArg }:instrs)
        _ ->
          (vars, instr:instrs)
      where
        replaceVar var = fromMaybe var $ Map.lookup var vars


-- |Replaces some instructions with calls to functions from glibc or custom
-- wrapper functions.
simplify :: SFunction -> SFunction
simplify func@SFunction{..}
  = mapI replace func
  where
    replace op@SBinOp{..}
      = case siBinOp of
          Div -> SCall [siDest] "__aeabi_idiv" [siLeft, siRight]
          Mod -> SCall [SVar (-1), siDest] "__aeabi_idivmode" [siLeft, siRight]
          x -> op
    replace op@SUnOp{..}
      = case siUnOp of
          Len -> SReadArray siDest siArg (SImm (-1)) Int
          x -> op
    replace SNewArray{..}
      = SCall [siDest] "__alloc" [SImm siLength]
    replace SNewPair{..}
      = SCall [siDest] "__alloc" [SImm 8]
    replace SFree{..}
      = SCall [] "__free" [siRef]
    replace x
      = x