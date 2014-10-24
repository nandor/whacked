{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.FlowGraph
  ( FlowGraph
  --, relabel
  , buildFlowGraph
  , allPathsReturn
  ) where


import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Safe
import           Whacked.Tree
import           Whacked.Itch
import           Whacked.Scratch
import           Whacked.Types



type FlowGraph
  = Map Int [Int]


-- |Checks if all the paths of the AST return. If not all paths return from
-- a function, a syntax error is thrown. The only messy case is represented
-- by if statements: either both paths return or there is a return after
-- the if statement. The statements that are considered terminal are exit
-- and return statements.
allPathsReturn :: [AStatement] -> Bool
allPathsReturn []
  = False
allPathsReturn (AIf{..}:ps)
  = (allPathsReturn asTrue && allPathsReturn asFalse) || allPathsReturn ps
allPathsReturn (ABlock{..}:ps)
  = allPathsReturn asBody || allPathsReturn ps
allPathsReturn (AReturn{}:ps)
  = True
allPathsReturn (AExit{}:ps)
  = True
allPathsReturn (p:ps)
  = allPathsReturn ps


-- |Builds the control flow graph. If there is an edge between two blocks i and
-- j, it means that there is a jump instruction between those two blocks.
buildFlowGraph :: SFunction -> (FlowGraph, FlowGraph)
buildFlowGraph func@SFunction{..}
  = (cfg, cfg')
  where
    cfg = Map.fromList (build . Map.toList $ sfBlocks)
    cfg' = Map.foldlWithKey rev Map.empty cfg

    build []
      = []
    build ((idx, SBlock{..}):(idx', y):bs)
      = case lastMay sbInstrs of
        Nothing           -> (idx, [idx']) : tail
        Just SBinJump{..} -> (idx, [idx', siWhere]) : tail
        Just SUnJump{..}  -> (idx, [idx', siWhere]) : tail
        Just SJump{..}    -> (idx, [siWhere]) : tail
        Just SReturn{..}  -> tail
        Just _            -> (idx, [idx']) : tail
      where
        tail = build $ (idx', y) : bs
    build ((idx, SBlock{..}):[])
      = case lastMay sbInstrs of
        Nothing           -> []
        Just SBinJump{..} -> [(idx, [siWhere])]
        Just SUnJump{..}  -> [(idx, [siWhere])]
        Just SJump{..}    -> [(idx, [siWhere])]
        Just SReturn{..}  -> []
        Just _            -> []

    rev cfg' node out
      = foldl (\cfg' x -> Map.insertWith (++) x [node] cfg') cfg' out


-- |Relabels the instructions after eliminating nodes so that all identifiers
-- range from 0 to n, where n is the number of instructions.
{-relabel :: SFunction -> SFunction
relabel func@SFunction{..}
  = func{ sfBody = map relabel' sfBody }
  where
    labels = Map.fromList (genLabels 0 0 (map fst sfBody))
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
-}