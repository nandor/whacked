{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.SCCP where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.Scratch


import Debug.Trace


data Value
  = Top
  | Bot
  | ConstInt Int
  deriving ( Eq, Ord, Show )


combine :: Value -> Value -> Value
combine Top x
  = x
combine x Top
  = x
combine x y
  | x == y = x
  | otherwise = Bot


-- | Builds the control flow graph.
buildCFGGraph :: [(Int, SInstr)] -> Map Int [Int]
buildCFGGraph block
  = Map.fromList . zip (map fst block) . map build $ block
  where
    (count, _) = last block

    build (idx, SReturn{..})
      = []
    build (idx, SJump{..})
      = [siWhere]
    build (idx, SBinJump{..})
      | idx < count = [siWhere, idx + 1]
      | otherwise = [siWhere]
    build (idx, SUnJump{..})
      | idx < count = [siWhere, idx + 1]
      | otherwise = [siWhere]
    build (idx, _)
      | idx < count = [idx + 1]
      | otherwise = []


-- | Builds the SSA graph.
buildSSAGraph :: [(Int, SInstr)] -> Map Int [Int]
buildSSAGraph block
  = foldl linkDefs Map.empty $ block
  where
    defs = foldl findDefs Map.empty block
    findDefs mp (idx, SBinOp{..})
      = Map.insert siDest idx mp
    findDefs mp (idx, SConstInt{..})
      = Map.insert siDest idx mp
    findDefs mp (idx, SPhi{..})
      = Map.insert siDest idx mp
    findDefs mp _
      = mp

    linkDefs mp (idx, SBinOp{..})
      = foldl (linkTo idx) mp [siLeft, siRight]
    linkDefs mp (idx, SCall{..})
      = foldl (linkTo idx) mp siArgs
    linkDefs mp (idx, SPhi{..})
      = foldl (linkTo idx) mp siMerge
    linkDefs mp (idx, SReturn{..})
      = foldl (linkTo idx) mp [siVal]
    linkDefs mp (idx, SBinJump{..})
      = foldl (linkTo idx) mp [siLeft, siRight]
    linkDefs mp (idx, SUnJump{..})
      = foldl (linkTo idx) mp [siVal]
    linkDefs mp _
      = mp

    linkTo idx mp var
      = case Map.lookup var defs of
        Nothing -> mp
        Just from -> Map.insertWith (++) from [idx] mp


optimise :: SFunction -> SFunction
optimise SFunction{..}
  = traceShow (ssaGraph) . SFunction $ sfBody
  where
    cfgGraph = buildCFGGraph sfBody
    ssaGraph = buildSSAGraph sfBody


sccp :: SProgram -> SProgram
sccp SProgram{..}
  = SProgram $ map optimise spFuncs