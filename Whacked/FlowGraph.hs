{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.FlowGraph
  ( FlowGraph
  , buildFlowGraph
  ) where


import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
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
    next
      = Map.fromList $ zip (map fst block) (tail . map (\(x, _) -> [x]) $ block)

    (count, _) = last block

    build (idx, SReturn{..})
      = []
    build (idx, SJump{..})
      = [siWhere]
    build (idx, SBinJump{..})
      | idx < count = siWhere : fromMaybe [] (Map.lookup idx next)
      | otherwise = [siWhere]
    build (idx, SUnJump{..})
      | idx < count = siWhere : fromMaybe [] (Map.lookup idx next)
      | otherwise = [siWhere]
    build (idx, _)
      | idx < count = fromMaybe [] (Map.lookup idx next)
      | otherwise = []

    rev cfg' node out
      = foldl (\cfg' x -> Map.insertWith (++) x [node] cfg') cfg' out