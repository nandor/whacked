{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Whacked.Backend.ARM.Allocator
  ( liveVariables
  , getPreferredRegs
  , allocRegs
  ) where

import           Control.Applicative
import           Data.List
import           Data.Function
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types
import           Whacked.Backend.ARM.ASM



liveVariables :: SFunction -> Map Int (Set SVar, Set SVar)
liveVariables func@SFlatFunction{..}
  = fst . last . takeWhile snd . iterate updateAll $ (initial, True)
  where
    -- Build up the control flow graph.
    cfg = foldl addLink next sfInstrs
    addLink mp (i, x) = Map.insertWith (++) i (getTarget x) mp
    next = Map.fromList (zip labels . map (:[]) $ tail labels)

    -- Some helper maps for easy lookup.
    labels = map fst sfInstrs
    initial = foldr (\x -> Map.insert x (Set.empty, Set.empty)) Map.empty labels
    instrs = Map.fromList sfInstrs

    updateAll (mp, _)
      = foldl update (mp, False) labels

    update (mp, modified) x = fromMaybe (mp, modified) $ do
      instr <- Map.lookup x instrs
      succ <- Map.lookup x cfg
      (oldIn, oldOut) <- Map.lookup x mp
      let newOut  = Set.unions [x | Just (x, _) <- map (`Map.lookup` mp) succ]
          kill    = Set.fromList (getKill instr)
          gen     = Set.fromList (getGen instr)
          newIn = Set.difference oldOut kill `Set.union` gen
      return
        ( Map.insert x (newIn, newOut) mp
        , modified || newIn /= oldIn || newOut /= oldOut
        )


-- |For each live variable in the program, computes a list of registers where
-- the variable can be placed. The registers are ordered by preference.
getPreferredRegs :: [([SVar], [SVar])] -> SFunction -> Map SVar [ARMLoc]
getPreferredRegs live func@SFlatFunction{..}
  = Map.filterWithKey (\x _ -> x `Set.member` liveSet)
  . Map.map nub
  . foldl clearArgs (foldl banRegs (foldl allowRegs args instrs) instrs)
  $ atArgs
  where
    -- Pairs of live variables & instructions.
    instrs = zip (map snd live) . map snd $ sfInstrs

    (atArgs, _):_ = live

    -- Set of live variables.
    liveSet
      = (Set.fromList . concatMap snd $ live)
        `Set.union`
        Set.fromList sfArgs

    -- Mapping of arguments.
    args
      = foldr (\(x, y) -> Map.insert x (y : argRegs ++ allRegs)) Map.empty
      $ zip sfArgs (argInLocation $ length sfArgs)

    -- For each live variable, associates the list of all registers as
    -- available.
    allowRegs mp (live, op@SCall{..})
      = putRegs live (getKill op ++ getGen op) allRegs mp
    allowRegs mp (live, op@SReturn{..})
      = putRegs live (getGen op) (ARMLocReg R0:allRegs) mp
    allowRegs mp (live, op)
      = putRegs live (getKill op ++ getGen op) (argRegs ++ allRegs) mp

    putRegs live vars regs mp
      = foldr (\x -> Map.insertWith (++) x regs) mp (vars `intersect` live)

    -- For function calls, bans all live variables from being placed in R0-R3,
    -- but allows return values to be placed there.
    banRegs mp (live, SCall{..})
      = foldl banArgs (foldl clearArgs mp live)
      $ (zip siArgs banIn) ++ (zip siRet banOut)
      where
        banIn
          = [ []
            , [ARMLocReg R0]
            , [ARMLocReg R0, ARMLocReg R1]
            , [ARMLocReg R0, ARMLocReg R1, ARMLocReg R2]
            ] ++
          repeat [ARMLocReg R0, ARMLocReg R1, ARMLocReg R2, ARMLocReg R3]
        banOut
          = [ [ARMLocReg R1, ARMLocReg R2, ARMLocReg R3]
            , [ARMLocReg R2, ARMLocReg R3]
            , [ARMLocReg R3]
            ] ++
          repeat []
        banArgs mp (reg, ban) = fromMaybe mp $ do
          regs <- nub <$> Map.lookup reg mp
          return $ Map.insert reg (regs \\ ban) mp
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
    allRegs = [ ARMLocReg x | x <- reverse $ enumFromTo R4 R9 ]
    argRegs = [ ARMLocReg x | x <- enumFromTo R0 R3 ]


-- |Tries to allocate hardware registers for all variables in the program.
allocRegs :: [([SVar], [SVar])] -> SFunction -> Map SVar [ARMLoc] -> Map SVar ARMLoc
allocRegs live func@SFlatFunction{..} pref
  = Map.union lowAlloc alloc
  where
    -- Counts the number each variable is used.
    useCount
      = foldl (\mp x -> Map.insertWith (+) x 1 mp) Map.empty
      . concatMap (getGen . snd)
      $ sfInstrs

    -- Only liveOut sets.
    live' = map snd live

    -- List of all live variables.
    liveVars = Set.toList . Set.fromList $ sfArgs ++ concat live'

    -- For each variable, generates a list of vars which conflict with it.
    conflict
      = foldl addConflicts Map.empty (live' ++ [sfArgs])
      where
        addConflicts mp xs
          = foldl addLink mp [(x, y) | x <- xs, y <- xs, x /= y]
        addLink mp (x, y)
          = Map.insertWith Set.union x (Set.singleton y) mp

    -- List of registers that can be mapped to R0 - R3.
    lowVars = sortr
      [ x
      | x <- liveVars, (Map.lookup x pref >>= minimum') < Just (ARMLocReg R4)
      ]
    minimum' []
      = Nothing
    minimum' xs
      = Just (minimum xs)

    -- Sorts the registers by number of uses.
    sortr = sortBy (flip compare `on` (`Map.lookup` useCount))

    -- First assign low vars to low regs.
    (lowAlloc, lowLeft, lowRegs)
      = assign lowVars [ARMLocReg x | x <- enumFromTo R0 R3]
    -- Then assign the rest to normal regs.
    (alloc, _, _)
      = assign (sortr $ lowLeft ++ (liveVars \\ lowVars))
      $ lowRegs ++
        [ARMLocReg x | x <- enumFromTo R4 R9] ++
        [ARMLocStk x | x <- [0..]]

    -- Checks if a var can be assigned to a reg.
    canAlloc v (ARMLocStk _)
      = True
    canAlloc v reg
      = elem reg . fromMaybe [] $ Map.lookup v pref

    -- Assigns variables to a candidate list of registers.
    assign vars []
      = (Map.empty, vars, [])
    assign [] regs
      = (Map.empty, [], regs)
    assign vars (r:rs)
      = (Map.union mp' mp'', vars'', regs'')
      where
        (mp', vars') = assign' vars r Set.empty
        (mp'', vars'', regs'') = assign vars' rs

        assign' (v:vs) reg set
          | not (canAlloc v reg) || Set.member v set
            = let (mp, vs') = assign' vs reg set in (mp, v:vs')
          | otherwise
            = let (mp, vs') = assign' vs reg (Set.union set ban)
              in (Map.insert v reg mp, vs')
          where
            ban = fromMaybe Set.empty $ Map.lookup v conflict
        assign' [] reg set
          = (Map.empty, [])