{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.FlowGraph
  ( FlowGraph
  , relabel
  , removePhi
  , buildFlowGraph
  ) where


import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Whacked.Scratch
import           Whacked.Types


import Debug.Trace

type FlowGraph
  = Map Int [Int]


-- | Relabels the instructions after eliminating nodes.
relabel :: [(Int, SInstr)] -> [(Int, SInstr)]
relabel code
  = map relabel' code
  where
    labels = Map.fromList (genLabels 0 0 (map fst code))
    genLabels n cnt (x:xs)
      = zip [n..] (replicate (x - n + 1) cnt) ++ genLabels (x + 1) (cnt + 1) xs
    genLabels n _ []
      = []

    update x = fromJust $ Map.lookup x labels

    relabel' (i, jmp@SBinJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, jmp@SUnJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, jmp@SJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, x)
      = (update i, x)


-- | Removes phi nodes from the code.
removePhi :: [(Int, SInstr)] -> [(Int, SInstr)]
removePhi code
  = [ instr | Just instr <- map removePhi' code ]
  where
    (alias, _) = foldr findAlias (Map.empty, 0) code
    findAlias (_, SPhi{..}) (alias, next)
      = case Map.lookup siDest alias of
          Nothing ->
            ( foldl (\mp x -> Map.insert x (SVar next) mp)
             (Map.insert siDest (SVar next) alias)
             siMerge
            , next + 1
            )
          Just var ->
            ( foldl (\mp x -> Map.insert x var mp) alias siMerge
            , next
            )
    findAlias (_, x) (alias, next)
      = foldl newAlias (alias, next) (getGen x)

    newAlias (alias, next) var
      = case Map.lookup var alias of
          Nothing -> ( Map.insert var (SVar next) alias, next + 1)
          Just _ -> ( alias, next )

    replace var
      = fromMaybe var $ Map.lookup var alias

    removePhi' (i, SPhi{..})
      = Nothing
    removePhi' (i, call@SCall{..})
      = Just (i, call{ siDest = replace siDest, siArgs = map replace siArgs })
    removePhi' (i, int@SConstInt{..})
      = Just (i, int{ siDest = replace siDest })
    removePhi' (i, jmp@SBinJump{..})
      = Just (i, jmp{ siLeft = replace siLeft, siRight = replace siRight })
    removePhi' (i, jmp@SUnJump{..})
      = Just (i, jmp{ siVal = replace siVal })
    removePhi' (i, jmp@SJump{..})
      = Just (i, jmp)
    removePhi' (i, bin@SBinOp{..})
      = Just (i, bin
          { siDest = replace siDest
          , siLeft = replace siLeft
          , siRight = replace siRight
          })


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