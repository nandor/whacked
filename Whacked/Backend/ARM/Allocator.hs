{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Allocator
  ( liveVariables
  ) where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types


import Debug.Trace


liveVariables :: SFlatFunction -> Map Int (Set SVar, Set SVar)
liveVariables func@SFlatFunction{..}
  = fst . last . takeWhile snd . iterate updateAll $ (initial, True)
  where
    -- Build up the control flow graph.
    cfg = foldl addLink next sffInstrs
    addLink mp (i, x) = Map.insertWith (++) i (getTarget x) mp
    next = Map.fromList (zip labels . map (:[]) $ tail labels)

    -- Some helper maps for easy lookup.
    labels = map fst sffInstrs
    initial = foldr (\x -> Map.insert x (Set.empty, Set.empty)) Map.empty labels
    instrs = Map.fromList sffInstrs

    updateAll (mp, _)
      = foldl update (mp, False) labels

    update (mp, modified) x = fromMaybe (mp, modified) $ do
      instr <- Map.lookup x instrs
      succ <- Map.lookup x cfg
      (oldIn, oldOut) <- Map.lookup x mp
      let newOut  = Set.unions [x | Just (x, _) <- map (`Map.lookup` mp) succ]
          kill    = Set.fromList (getKill instr)
          gen     = Set.fromList (getGen instr)
          newIn = (Set.difference oldOut kill) `Set.union` gen
      return
        ( Map.insert x (newIn, newOut) mp
        , modified || newIn /= oldIn || newOut /= oldOut
        )