{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns #-}
module Whacked.Optimizer.RemovePHI
  ( removePhi
  ) where

import           Control.Applicative
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set as Set
import           Whacked.FlowGraph
import           Whacked.Scratch



-- |Computes congruence classes. Each element will be mapped to a single
-- element by which it is replaced. This works only if we do not do copy
-- folding on the SSA graph.
congruence :: SFunction -> Map SVar SVar
congruence func@SFunction{ sfBlocks }
  = Map.map fst trees
  where
    -- All concruence classes
    trees = Map.foldl (\mp b -> foldl insert mp $ sbPhis b) Map.empty sfBlocks

    -- Unites all variables in PHI expressions.
    insert mp SPhi{..}
      = let vars = spDest : spMerge
        in foldl union mp [(x, y) | x <- vars, y <- vars, x < y]

    -- Disjoint set union.
    union set (a, b)
      | aParent == bParent
        = set
      | aDegree > bDegree
        = Map.insert bParent (aParent, bDegree) set
      | aDegree < bDegree
        = Map.insert aParent (bParent, aDegree) set
      | aDegree == bDegree
        = Map.insert aParent (bParent, aDegree)
        . Map.insert bParent (bParent, bDegree + 1)
        $ set
      where
        (aParent, aDegree) = find set a
        (bParent, bDegree) = find set b

    -- Disjoint set lookup.
    find set x = case Map.lookup x set of
      Nothing                -> (x, 0)
      Just (x', d) | x' == x -> (x, d)
      Just (x', d)           -> find set x'


-- |Removes all phi nodes.
removePhi :: SFunction -> SFunction
removePhi func@SFunction{..}
  = func{ sfBlocks = Map.map removeBlock sfBlocks }
  where
    -- |Merge everything from the same congruence class.
    cong = congruence func
    replace i
      | Just i' <- Map.lookup i cong = i'
      | otherwise = i

    removeBlock block@SBlock{..}
      = block{ sbPhis = [], sbInstrs = map replaceInstr sbInstrs }

    replaceInstr op@SBinOp{..}
      = op
        { siLeft = replace siLeft
        , siRight = replace siRight
        , siDest = replace siDest
        }
    replaceInstr op@SUnOp{..}
      = op{ siDest = replace siDest, siArg = replace siArg }
    replaceInstr op@SMov{..}
      = op{ siDest = replace siDest, siArg = replace siArg }
    replaceInstr op@SCall{..}
      = op{ siRet = map replace siRet, siArgs = map replace siArgs }
    replaceInstr op@SBool{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SChar{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SInt{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SBoolArray{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SCharArray{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SIntArray{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SNewArray{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SWriteArray{..}
      = op{ siExpr = replace siExpr
          , siArray = replace siArray
          , siIndex = replace siIndex
          }
    replaceInstr op@SReadArray{..}
      = op{ siDest = replace siDest
          , siArray = replace siArray
          , siIndex = replace siIndex
          }
    replaceInstr op@SNewPair{..}
      = op{ siDest = replace siDest }
    replaceInstr op@SWritePair{..}
      = op{ siPair = replace siPair, siExpr = replace siExpr }
    replaceInstr op@SReadPair{..}
      = op{ siDest = replace siDest, siPair = replace siPair }
    replaceInstr op@SFree{..}
      = op{ siRef = replace siRef }
    replaceInstr op@SReturn{..}
      = op{ siArg = replace siArg }
    replaceInstr op@SBinJump{..}
      = op{ siLeft = replace siLeft, siRight = replace siRight }
    replaceInstr op@SUnJump{..}
      = op{ siArg = replace siArg }
    replaceInstr op@SJump{..}
      = op