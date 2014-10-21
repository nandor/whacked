{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Whacked.Backend.ARM.Allocator
  ( allocate
  ) where

import           Data.Function
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Types
import           Whacked.Backend.ARM.ASM
import           Whacked.LiveVariables
import           Whacked.Scratch


import Debug.Trace


-- Function which mas symbolic variables to hardware registers or stack indices.
-- The function first does live variable analysis in order to compute which
-- values require storage at a certain point in a program. An interference
-- graph is built out of this information where an edge between two nodes means
-- that the values must be stored in different registers. For each variable, a
-- set of optimal registers is computed.
-- For arguments, the optimal registers are r0 - r3, but they might need to be
-- relocated if those registers are used in a function call. Similarly, the
-- optimal value of a variable used in a return statement is r0.
-- If a function call is made at a point in a program, all the variables
-- live at that point in the program are prevented from being assigned to
-- r0 - r3, unless they go out of scope for the rest of the program.
allocate :: Map Int (Set SVar, Set SVar) -> SFunction -> Map SVar ARMReg
allocate live func@SFunction{..}
  = Map.fromList . assign varCount $ Set.empty
  where
    -- Counts the number of times each variable is read from memory.
    varCount
      = map fst . sortBy comp . Map.toList . foldl count Map.empty $ sfBody
      where
        count mp (_, instr)
          = foldl (\mp x -> Map.insertWith (+) x 1 mp) mp (getGen instr)
        comp (_, x) (_, y)
          = y `compare` x

    -- For each variable, builds a list of preferred location.
    varPref
      = removeConflicts $ Map.map Set.fromList . foldl pref argsPref $ sfBody
      where
        pref mp (_, SBinOp{..})
          = Map.insertWith (++) siDest allRegs mp
        pref mp (_, SCall{..})
          = foldl add mp $ zip siRet (enumFrom R0) ++ zip siArgs (enumFrom R0)
          where
            add mp (var, reg)
              = Map.insertWith (++) var (reg : allRegs) mp
        pref mp (_, SConstInt{..})
          = Map.insertWith (++) siDest allRegs mp
        pref mp (_, SConstBool{..})
          = Map.insertWith (++) siDest allRegs mp
        pref mp (_, SReturn{..})
          = Map.insertWith (++) siVal (R0 : allRegs) mp
        pref mp (_, instr)
          = mp

        argsPref
          = foldl insert Map.empty $ zip sfArgs (enumFromTo R0 R11)
          where
            insert mp (arg, pref)
              = Map.insertWith (++) arg (pref : allRegs) mp

        removeConflicts mp
          = foldl remove mp sfBody
          where
            remove mp (i, SCall{..})
              = foldl removeArgs mp (Set.toList liveOut)
              where
                Just (_, liveOut) = Map.lookup i live
            remove mp (_, _)
              = mp
            removeArgs mp arg
              = Map.insert arg (oldRegs `Set.difference` argRegs) mp
              where
                Just oldRegs = Map.lookup arg mp

    -- All common registers.
    allRegs
      = enumFromTo R4 R11
    argRegs
      = Set.fromList (enumFromTo R0 R3)

    graph
      = foldl addEdge Map.empty (map (snd . snd) . Map.toList $ live)
      where
        addEdge mp liveOut
          = foldr (\(x, y) -> Map.insertWith Set.union x (Set.singleton y)) mp
              [ (x, y)
              | x <- vars
              , y <- vars
              , x /= y
              ]
          where
            vars = Set.toList liveOut

    assign [] _
      = []
    assign (x:xs) used
      = assign' (fromMaybe Set.empty $ Map.lookup x graph) (x:xs) []
      where
        (r:rs)
          | Just x <- Map.lookup x varPref
            = Set.toList (x `Set.difference` used)
          | otherwise
            = error "This should not happen."

        assign' set (x:xs) ys
          | Set.member x set = assign' set xs (x:ys)
          | otherwise = (x, r) : assign' (set `Set.union` set') xs ys
          where
            set' = fromMaybe Set.empty $ Map.lookup x graph
        assign' set [] ys
          = assign ys (Set.insert r used)
