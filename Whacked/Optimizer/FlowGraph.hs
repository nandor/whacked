{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.FlowGraph
  ( FlowGraph
  , buildFlowGraph
  ) where


import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.Scratch
import           Whacked.Types



type FlowGraph
  = Map Int [Int]


-- | Builds the control flow graph.
buildFlowGraph :: [(Int, SInstr)] -> (FlowGraph, FlowGraph)
buildFlowGraph block
  = (cfg, cfg')
  where
    cfg = Map.fromList . zip (map fst block) . map build $ block
    cfg' = Map.foldlWithKey rev Map.empty cfg

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

    rev cfg' node out
      = foldl (\cfg' x -> Map.insertWith (++) x [node] cfg') cfg' out