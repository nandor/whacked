{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, TupleSections #-}
module Whacked.Optimizer.SCCP where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List (nub, (\\))
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types
import           Whacked.FlowGraph


import Debug.Trace


-- |Values used in the lattice for sparse conditional constant propagation.
-- Due to the fact that the input code is type checked, when combining two
-- values that are neither Bot nor Top, both of them will have the same type.
-- Pair references can either be null or point to an original pair.
-- The code will try to merge most references to the original svar that was
-- the destination of the instruction that allocated the pair.
-- Array references can either be null or point to an original array
-- with length information. The original array refers to the svar that was
-- the destination of the instruction that allocated this array.
data Value
  = Top
  | Bot
  | CInt Int
  | CBool Bool
  | CChar Char
  | CArray Value Value
  | CRef (Maybe SVar)
  deriving ( Eq, Ord, Show )


-- |The monoid instance is used when aggregating values that come from
-- different PHI nodes. Top values are ignored, disjoint values yield Bot.
instance Monoid Value where
  mappend Top x = x
  mappend x Top = x
  mappend (CArray refA lenA) (CArray refB lenB)
    | ref' == Bot && len' == Bot = Bot
    | otherwise = CArray ref' len'
    where
      ref' = refA <> refB
      len' = lenA <> lenB
  mappend x y
    | x == y = x
    | otherwise = Bot
  mempty
    = Top


-- |Compares two values. Returns a boolean if both values are known and
-- can be compared.
compareValue :: CondOp -> Value -> Value -> Maybe Bool
compareValue _ Bot _
  = Nothing
compareValue _ _ Bot
  = Nothing
compareValue op (CInt x)  (CInt y)
  = Just $ compareOp op x y
compareValue op (CBool x) (CBool y)
  = Just $ compareOp op x y
compareValue op (CChar x) (CChar y)
  = Just $ compareOp op x y
compareValue op (CRef ref) (CRef ref')
  = Just $ compareOp op ref ref'
compareValue op (CArray ref _) (CArray ref' _)
  = compareValue op ref ref'


-- |Evaluates a binary operation. If the result is well defined, it returns a
-- value describing it. If one of the operands was Top or the values are not
-- consistent, Nothing is returned.
evalBinOp :: BinaryOp -> Value -> Value -> Value
evalBinOp _ Bot _
  = Bot
evalBinOp _ _ Bot
  = Bot
evalBinOp _ Top y
  = y
evalBinOp _ y Top
  = y
evalBinOp op (CInt x) (CInt y)
  = case op of
    Add          -> CInt (x + y)
    Sub          -> CInt (x - y)
    Mul          -> CInt (x * y)
    Div | y /= 0 -> CInt (x `div` y)
    Div          -> Bot
    Mod | y /= 0 -> CInt (x `mod` y)
    Mod          -> Bot
    Cmp cmp      -> CBool $ compareOp cmp x y
evalBinOp op (CBool x) (CBool y)
  = case op of
    Or      -> CBool (x || y)
    And     -> CBool (x && y)
    Cmp cmp -> CBool $ compareOp cmp x y
evalBinOp op (CChar x) (CChar y)
  = case op of
    Cmp cmp -> CBool $ compareOp cmp x y
evalBinOp op (CRef ref) (CRef ref')
  = case op of
    Cmp CEQ -> CBool $ ref == ref'
    Cmp CNE -> CBool $ ref /= ref'
evalBinOp op (CArray ref _) y'@(CArray ref' _)
  = case op of
    Cmp CEQ -> fromMaybe Top $ CBool <$> compareValue CEQ ref ref'
    Cmp CNE -> fromMaybe Top $ CBool <$> compareValue CNE ref ref'


-- |Evaluates an unary operation.
evalUnOp :: UnaryOp -> Value -> Value
evalUnOp _ Bot
  = Bot
evalUnOp _ Top
  = Top
evalUnOp Neg (CInt x)            = CInt (-x)
evalUnOp Not (CBool x)           = CBool (not x)
evalUnOp Chr (CInt x)            = CChar (chr x)
evalUnOp Ord (CChar x)           = CInt (ord x)
evalUnOp Len (CArray _ (CInt x)) = CInt x
evalUnOp Len _                   = Bot
evalUnOp _ _                     = Bot


-- |Builds the SSA graph, linking defitions to uses.
buildSSAGraph :: SFunction -> (Map Int [SVar], Map Int [Int])
buildSSAGraph func@SFunction{..}
  = (blockDefs, Map.mapWithKey (\k v -> nub v) ssa)
  where
    -- Find the definition point of all variables.
    defs = Map.foldlWithKey foldFunc Map.empty sfBlocks
    foldFunc mp idx SBlock{ sbInstrs, sbPhis }
      = foldl (getDefs idx)
        (foldl (\mp x -> Map.insert (spDest x) idx mp) mp sbPhis)
      $ sbInstrs
    getDefs i mp instr
      = foldl (\mp x -> Map.insert x i mp) mp (getKill instr)

    -- Link definitions to all uses.
    (blockDefs, ssa)
      = Map.foldlWithKey linkFunc (Map.empty, Map.empty) sfBlocks
    linkFunc (mp, mp') idx SBlock{ sbInstrs, sbPhis }
      = ( Map.insert idx allDefs mp
        , foldl (linkDefs idx) mp' allDefs
        )
      where
        allDefs
          = concat $ (map (map snd . spMerge) sbPhis) ++ (map getGen sbInstrs)
    linkDefs i mp var
      = case Map.lookup var defs of
          Nothing -> mp
          Just def -> Map.insertWith (++) def [i] mp


-- |Performs sparse conditional constant propagation.
sccp :: SFunction -> SFunction
sccp func@SFunction{..}
  = func{ sfBlocks = Map.fromList . prune . Map.toList $ sfBlocks }
  where
    (cfgGraph, cfgGraph') = buildFlowGraph func
    (blockDefs, ssaGraph) = buildSSAGraph func
    (start, _) = Map.findMin sfBlocks
    varsInitial = Map.fromList $
      (zip (vars \\ sfArgs) . repeat $ Top) ++
      (zip sfArgs . repeat $ Bot)
    vars = concat . map snd . Map.toList $ blockDefs

    -- Removes unrechable blocks & replaces constants.
    prune []
      = []
    prune ((idx, block@SBlock{..}):xs)
      | not $ idx `Set.member` marked = prune xs
      | otherwise =
          ( idx
          , block{ sbInstrs = [x | Just x <- map simplifyInstr sbInstrs] }
          ) : xs'
      where
        xs' = prune xs

        toNext
          = case xs' of
          []              -> Nothing
          ((idx', _) : _) -> Just $ SJump idx'

        simplifyInstr op@SBinJump{..} = do
          left <- lookupValue vars' siLeft
          right <- lookupValue vars' siRight
          case compareValue siCond left right of
            Just True  -> Just $ SJump siWhere
            Just False -> toNext
            Nothing    -> Just op
        simplifyInstr op@SUnJump{..} = do
          arg <- lookupValue vars' siArg
          case arg of
            CBool x | x == siWhen -> Just $ SJump siWhere
            CBool x | x /= siWhen -> toNext
            _                     -> Just op
        simplifyInstr op@SBinOp{..} = do
          left <- lookupValue vars' siLeft
          right <- lookupValue vars' siRight
          case evalBinOp siBinOp left right of
            CInt x  -> Just $ SInt siDest x
            CBool x -> Just $ SBool siDest x
            CChar x -> Just $ SChar siDest x
            _       -> Just op
        simplifyInstr x
          = Just x



    nextBlocks
      = let indices = map fst . Map.toList $ blockDefs
        in Map.fromList . zip indices $ tail indices
    next end
      = case Map.lookup end nextBlocks of
          Nothing -> []
          Just x -> [(end, x)]
    ssaNext end
      = fromMaybe [] $ map (end,) <$> Map.lookup end ssaGraph

    (executed, vars') = sccp' [(start, start)] [] Set.empty varsInitial
    marked = Set.fromList . map snd . Set.toList $ executed


    -- Sparse conditional propagation algorithm. Maintains two workflows:
    -- cfg for propagating through blocks and ssa for propagating values
    -- into new blocks.
    sccp' (edge@(start, end):cfg) ssa executed vars
      | Set.member edge executed
        -- If the edge was already executed, stop propagation and continue
        -- with other avilable edges.
        = sccp' cfg ssa executed vars
      | anyOtherExecuted executed start end
        -- If any other edges were executed, evaluate only phi instructions.
        = sccp' cfg (ssa0 ++ ssa) executed vars'
      | otherwise
        -- If no other edges were executed, evaluate everything.
        = sccp' (cfg' ++ cfg) (ssa0 ++ ssa1 ++ ssa) executed' vars''
      where
        executed' = Set.insert edge executed
        cfg' = if cfg0 ++ cfg1 == [] then next end else cfg0 ++ cfg1
        (cfg0, ssa0, vars')
          = evaluatePhis edge executed' vars
        (cfg1, ssa1, vars'')
          = evaluateInstrs edge executed' vars'

    sccp' [] (edge@(start, end):ssa) executed vars
      | not (anyExecuted executed end)
        -- When none of the edges entering a block were executed, stop
        -- propagating values through the SSA graph.
        = sccp' [] ssa executed vars
      | otherwise
        -- Otherwise, evaluate all instructions that take arguments from the
        -- start block.
        = sccp' (cfg0 ++ cfg1) (ssa0 ++ ssa1 ++ ssa) executed vars''
      where
        (cfg0, ssa0, vars')
          = evaluatePhis edge executed vars
        (cfg1, ssa1, vars'')
          = evaluateInstrs edge executed vars'

    sccp' [] [] executed vars
      = (executed, vars)


    -- Evaluates all phi nodes from a block.
    evaluatePhis edge@(start, end) executed vars
      = foldl addPhi ([], [], vars) phis
      where
        Just SBlock{..} = Map.lookup end sfBlocks
        phis = map evalPhi sbPhis

        evalPhi SPhi{..}
          = (spDest, merge)
          where
            merge
              = mconcat
              . map (fromMaybe Top . (lookupValue vars) . snd)
              . filter (\(x, _) -> (x, end) `Set.member` executed)
              $ spMerge

        addPhi (cfg, ssa, vars) (var, val)
          = case Map.lookup var vars of
            Just x | x == val || x == Bot ->
              (cfg, ssa, vars)
            _ ->
              ( cfg
              , ssa ++ ssaNext end
              , Map.insert var val vars
              )

    -- Evaluates all instructions from a block.
    evaluateInstrs edge@(start, end) executed vars
      = foldl evalInstr ([], [], vars) sbInstrs
      where
        Just SBlock{..} = Map.lookup end sfBlocks

        evalInstr arg@(cfg, ssa, vars) SBinOp{..} = fromMaybe arg $ do
          left <- lookupValue vars siLeft
          right <- lookupValue vars siRight
          let result = evalBinOp siBinOp left right
          return $ case lookupValue vars siDest of
            Just x | x == result || x == Bot ->
              (cfg, ssa, vars)
            _ ->
              (cfg, ssa ++ ssaNext end, Map.insert siDest result vars)

        evalInstr arg@(cfg, ssa, vars) SUnOp{..} = fromMaybe arg $ do
          arg <- lookupValue vars siArg
          let result = evalUnOp siUnOp arg
          return $ case lookupValue vars siDest of
            Just x | x == result || x == Bot ->
              (cfg, ssa, vars)
            _ ->
              (cfg, ssa ++ ssaNext end, Map.insert siDest result vars)

        evalInstr (cfg, ssa, vars) SMov{..}
          | lookupValue vars siDest == other = (cfg, ssa, vars)
          | otherwise
            = ( cfg
              , ssa ++ ssaNext end
              , Map.insert siDest (fromMaybe Top other) vars
              )
          where
            other = lookupValue vars siArg

        evalInstr (cfg, ssa, vars) SCall{..}
          | all (Just Bot ==) $ map (lookupValue vars) siRet = (cfg, ssa, vars)
          | otherwise =
            ( cfg
            , ssa ++ ssaNext end
            , foldl (\mp x -> Map.insert x Bot mp) vars siRet
            )

        evalInstr arg@(cfg, ssa, vars) op@SBinJump{..} = fromMaybe arg $ do
          left <- lookupValue vars siLeft
          right <- lookupValue vars siRight
          return $ case compareValue siCond left right of
            Just True  -> ((end, siWhere) : cfg, [], vars)
            Just False -> (next end ++ cfg, [], vars)
            Nothing    -> ((end, siWhere) : next end ++ cfg, [], vars)

        evalInstr arg@(cfg, ssa, vars) SUnJump{..}
          = case lookupValue vars siArg of
            Just (CBool x) | x == siWhen ->
              ((end, siWhere) : cfg, [], vars)
            Just (CBool x) ->
              (next end ++ cfg, [], vars)
            _ ->
              ((end, siWhere) : next end ++ cfg, [], vars)

        evalInstr (cfg, ssa, vars) SJump{..}
          = ((end, siWhere) : cfg, [], vars)

        evalInstr (cfg, ssa, vars) SReturn{..}
          = (cfg, ssa, vars)
        evalInstr (cfg, ssa, vars) SWriteArray{..}
          = (cfg, ssa, vars)
        evalInstr (cfg, ssa, vars) SWritePair{..}
          = (cfg, ssa, vars)

        evalInstr (cfg, ssa, vars) const
          = case lookupValue vars $ siDest const of
            Just x ->
              (cfg, ssa, vars)
            Nothing ->
              (cfg, ssa ++ ssaNext end, Map.insert (siDest const) const' vars)
          where
            const' = case const of
              SBool{..}       -> CBool siBool
              SChar{..}       -> CChar siChar
              SInt{..}        -> CInt siInt
              SString{..}     -> CRef $ Just siDest
              SNewArray{..}   -> CArray (CRef $ Just siDest) (CInt $ siLength)
              SNewPair{..}    -> CRef $ Just siDest
              SFree{..}       -> CRef Nothing
              SReadArray{..}  -> Bot
              SReadPair{..}   -> Bot

    -- Finds a value in the var pool. Only returns it if is well defined.
    lookupValue vars var
      = Map.lookup var vars >>= \case
          Top -> Nothing
          x -> Just x

    -- Checks if any edges going to any other than (start, end) were marked
    -- as executed previously.
    anyOtherExecuted mark start end = fromMaybe False $ do
      nodes <- Map.lookup end cfgGraph'
      return
        . any (\x -> x /= (start, end) && Set.member x mark)
        . zip nodes
        $ repeat end

    -- Checks if any of the edges entering the node were executed or not.
    anyExecuted mark end = fromMaybe False $ do
      nodes <- Map.lookup end cfgGraph'
      return
        . any (\x -> Set.member (x, end) mark)
        $ nodes
