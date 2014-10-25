{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Backend.ARM.Allocator
  ( liveVariables
  , getPreferredRegs
  , allocRegs
  ) where

import           Control.Applicative
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types
import           Whacked.Backend.ARM.ASM

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


-- |For each live variable in the program, computes a list of registers where
-- the variable can be placed. The registers are ordered by preference.
getPreferredRegs :: [[SVar]] -> SFlatFunction -> Map SVar [ARMReg]
getPreferredRegs live func@SFlatFunction{..}
  = Map.map nub $ foldl banRegs (foldl allowRegs Map.empty instrs) instrs
  where
    -- Pairs of live variables & instructions.
    instrs = zip live . map snd $ sffInstrs

    -- For each live variable, associates the list of all registers as
    -- available.
    allowRegs mp (live, op@SCall{..})
      = foldr (\(x, y) -> Map.insertWith (++) x [y])
        (putRegs live (getKill op ++ getGen op) allRegs mp)
      $ zip siArgs argRegs
    allowRegs mp (live, op@SReturn{..})
      = putRegs live (getGen op) (R0:allRegs) mp
    allowRegs mp (live, op)
      = putRegs live (getKill op ++ getGen op) allRegs mp

    putRegs live vars regs mp
      = foldr (\x -> Map.insertWith (++) x regs) mp (vars `intersect` live)

    -- For function calls, bans all live variables from being placed in R0-R3,
    -- but allows return values to be placed there.
    banRegs mp (live, SCall{..})
      = foldr (\(x, y) -> Map.insertWith (++) x [y])
        (foldl clearArgs mp live)
      $ zip siRet argRegs
    banRegs mp (live, x)
      = mp

    -- Removes argument registers from the prefferd list of variables live at
    -- the point of function calls.
    clearArgs mp var = fromMaybe mp $ do
      vars <- Map.lookup var mp
      return $ Map.insert var (removeAll vars argRegs) mp

    removeAll (x:xs) ban
      | x `elem` ban = removeAll xs ban
      | otherwise = x : removeAll xs ban
    removeAll [] _
      = []

    -- List of all registers usable for local variable storage. This is reversed
    -- so local variables will be placed in higher registers first. Function
    -- arguments & return values will have R0-R3 first, so the allocator will
    -- know that those are their preferred registers.
    allRegs
      = reverse $ enumFromTo R0 R11

    -- Registers used to pass arguments to functions.
    argRegs
      = enumFromTo R0 R3


-- |Tries to allocate hardware registers for all variables in the program.
allocRegs :: [[SVar]] -> SFlatFunction -> Map SVar [ARMReg] -> Map SVar ARMReg
allocRegs live func@SFlatFunction{..} pref
  = Map.empty
