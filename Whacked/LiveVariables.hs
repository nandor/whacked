{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.LiveVariables
  ( liveVariables
  ) where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.FlowGraph
import           Whacked.Scratch
import           Whacked.Types


-- | Function to perform data flow analysis.
framework :: (Eq a, Ord a)
          => (FlowGraph, FlowGraph)
          -> [Int]
          -> Map Int (Set a, Set a)
          -> Map Int (Set a, Set a)
framework (cfg, cfg') labels killGen
  = loop Map.empty
  where
    loop mp
      | modified = loop mp'
      | otherwise = mp'
      where
        (mp', modified) = foldl process (mp, False) labels

    process (mp, modified) idx
      = case Map.lookup idx mp of
          Nothing ->
            (mp', True)
          Just (oldIn, oldOut) ->
            (mp', modified || oldIn /= newIn || oldOut /= newOut)
      where
        mp' = Map.insert idx (newIn, newOut) mp

        Just (kill, gen) = Map.lookup idx killGen

        newIn = case snd <$> Map.lookup idx mp of
          Nothing -> gen
          Just out -> Set.union gen (Set.difference out kill)
        newOut = Set.unions $ case Map.lookup idx cfg of
          Nothing ->
            [Set.empty]
          Just succ ->
            map (\x -> fromMaybe Set.empty (fst <$> Map.lookup x mp)) succ


-- Finds the live variables at every point in a program.
liveVariables :: SFunction -> Map Int (Set SVar, Set SVar)
liveVariables func@SFunction{..}
  = framework (buildFlowGraph sfBody) labels $ vars
  where
    labels = map fst sfBody
    vars = Map.fromList . map findKillGen $ sfBody

    findKillGen (idx, op)
      = (idx, (Set.fromList (getKill op), Set.fromList (getGen op)))
