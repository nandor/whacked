{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
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
evalBinOp :: BinaryOp -> Value -> Value -> Maybe Value
evalBinOp _ Bot _
  = Nothing
evalBinOp _ _ Bot
  = Nothing
evalBinOp _ Top y
  = Just y
evalBinOp _ y Top
  = Just y
evalBinOp op (CInt x) (CInt y)
  = Just $ case op of
    Add     -> CInt (x + y)
    Sub     -> CInt (x - y)
    Mul     -> CInt (x * y)
    Div     -> CInt (x `div` y)
    Mod     -> CInt (x `mod` y)
    Cmp cmp -> CBool $ compareOp cmp x y
evalBinOp op (CBool x) (CBool y)
  = Just $ case op of
    Or      -> CBool (x || y)
    And     -> CBool (x && y)
    Cmp cmp -> CBool $ compareOp cmp x y
evalBinOp op (CChar x) (CChar y)
  = Just $ case op of
    Cmp cmp -> CBool $ compareOp cmp x y
evalBinOp op (CRef ref) (CRef ref')
  = Just $ case op of
    Cmp CEQ -> CBool $ ref == ref'
    Cmp CNE -> CBool $ ref /= ref'
evalBinOp op (CArray ref _) y'@(CArray ref' _)
  = case op of
    Cmp CEQ -> CBool <$> compareValue CEQ ref ref'
    Cmp CNE -> CBool <$> compareValue CNE ref ref'


-- |Evaluates an unary operation.
evalUnOp :: UnaryOp -> Value -> Maybe Value
evalUnOp _ Bot
  = Nothing
evalUnOp _ Top
  = Just Top
evalUnOp op (CInt x)
  = case op of
    Neg -> Just $ CInt (-x)
    Chr -> Just $ CChar (chr x)
evalUnOp op (CBool x)
  = case op of
    Not -> Just $ CBool (not x)
evalUnOp op (CChar x)
  = case op of
    Ord -> Just $ CInt (ord x)


-- |Builds the SSA graph, linking defitions to uses.
buildSSAGraph :: SFunction -> (Map Int [SVar], Map Int [Int])
buildSSAGraph func@SFunction{..}
  = (blockDefs, Map.mapWithKey (\k v -> nub v \\ [k]) ssa)
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
  = traceShow (func, vars') $ func
  where
    (cfg, cfg') = buildFlowGraph func
    (blockDefs, ssa) = buildSSAGraph func
    (start, _) = Map.findMin sfBlocks
    vars = concat . map snd . Map.toList $ blockDefs

    (executed, vars') = sccp'
      [(start, start)]                         -- Edge entering the start node.
      []                                       -- No SSA edges.
      Set.empty                                -- All edges unexecuted.
      (Map.fromList . zip vars . repeat $ Top) -- All vars set to TOP.


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
        = sccp' (cfg0 ++ cfg1 ++ cfg) (ssa0 ++ ssa1 ++ ssa) executed' vars''
      where
        executed' = Set.insert edge executed
        ssa' = ssa0 ++ ssa1
        cfg' = cfg0 ++ ssa0
        (cfg0, ssa0, vars')
          = evaluatePhis edge executed vars
        (cfg1, ssa1, vars'')
          = evaluateInstrs edge executed vars

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
        executed' = Set.insert edge executed
        (cfg0, ssa0, vars')
          = evaluatePhis edge executed vars
        (cfg1, ssa1, vars'')
          = evaluateInstrs edge executed vars

    sccp' [] [] executed vars
      = (executed, vars)


    -- Evaluates all phi nodes from a block.
    evaluatePhis edge@(start, end) executed vars
      = ([], [], traceShow "PHI" $ vars)
      where
        Just SBlock{..} = Map.lookup end sfBlocks

    -- Evaluates all instructions from a block.
    evaluateInstrs edge@(start, end) executed vars
      = foldl evalInstr ([], [], vars) sbInstrs
      where
        Just SBlock{..} = Map.lookup end sfBlocks

        evalInstr (cfg, ssa, vars) SBinOp{..}
          = traceShow "SBinOp" undefined
        evalInstr (cfg, ssa, vars) SUnOp{..}
          = traceShow "SUnOp" undefined
        evalInstr (cfg, ssa, vars) SMov{..}
          = traceShow "SMov" undefined
        evalInstr (cfg, ssa, vars) SCall{..}
          = traceShow "SCall" undefined

        evalInstr (cfg, ssa, vars) SBool{..}
          = (cfg, ssa, Map.insert siDest (CBool siBool) vars)
        evalInstr (cfg, ssa, vars) SChar{..}
          = (cfg, ssa, Map.insert siDest (CChar siChar) vars)
        evalInstr (cfg, ssa, vars) SInt{..}
          = (cfg, ssa, Map.insert siDest (CInt siInt) vars)

        evalInstr (cfg, ssa, vars) SString{..}
          = traceShow "SString" undefined
        evalInstr (cfg, ssa, vars) SNewArray{..}
          = traceShow "SNewArray" undefined
        evalInstr (cfg, ssa, vars) SWriteArray{..}
          = traceShow "SWriteArray" undefined
        evalInstr (cfg, ssa, vars) SReadArray{..}
          = traceShow "SReadArray" undefined
        evalInstr (cfg, ssa, vars) SNewPair{..}
          = traceShow "SNewPair" undefined
        evalInstr (cfg, ssa, vars) SWritePair{..}
          = traceShow "SWritePair" undefined
        evalInstr (cfg, ssa, vars) SReadPair{..}
          = traceShow "SReadPair" undefined
        evalInstr (cfg, ssa, vars) SFree{..}
          = traceShow "SFree" undefined
        evalInstr (cfg, ssa, vars) SReturn{..}
          = traceShow "SReturn" undefined
        evalInstr (cfg, ssa, vars) SBinJump{..}
          = traceShow "SBinJump" undefined
        evalInstr (cfg, ssa, vars) SUnJump{..}
          = traceShow "SUnJump" undefined
        evalInstr (cfg, ssa, vars) SJump{..}
          = traceShow "SJump" undefined

    {-traverse (edge@(start, end):xs) ys mark vars
      = case Map.lookup end code of
        _ | Set.member edge mark ->
          traverse xs ys mark' vars

        -- Evaluate phi nodes & propagate to the next node.
        Just node@SPhi{..} ->
          let (xs', ys', var') = traverse' node next edge mark vars
          in traverse (xs' ++ xs) (ys' ++ ys) (Set.insert edge mark) var'

        -- If any other edges were executed and the node is not a phi node,
        -- skip this node and continue on with others.
        _ | anyOtherExecuted mark start end ->
          traverse xs ys mark' vars

        -- Otherwise, evaluate the new node and continue.
        Just node ->
          let (xs', ys', var') = traverse' node next edge mark vars
          in traverse (xs' ++ xs) (ys' ++ ys) (Set.insert edge mark) var'
      where
        mark' = Set.insert edge mark
        next = zip (repeat end) $ fromMaybe [] (Map.lookup end cfg)

    traverse [] (edge@(start, end):ys) mark vars
      = case Map.lookup end code of
          -- If none of the incoming edges were executed, postpone evaluation.
          _ | not $ anyExecuted mark end ->
            traverse [] ys mark vars
          Just node ->
            let (xs', ys', vars') = traverse' node [] edge mark vars
            in traverse xs' (ys' ++ ys) mark vars'

    traverse [] [] mark vars
      = (mark, vars)

    traverse' node xs (start, end) mark vars = case node of
      -- Evaluate a phi node. When evaluating a phi node, we take into
      -- consideration the edges that come into the node and are marked
      -- executable. The meet function is then applied over those values.
      node@SPhi{..} ->
        let new = mconcat [fromMaybe Top (Map.lookup x vars) | x <- siMerge]
        in if new == (fromJust $ Map.lookup siDest vars)
            then
              ( xs, [], vars )
            else
              ( xs
              , ssaNext
              , Map.insert siDest new vars
              )

      -- Evaluate a binary operation.
      node@SBinOp{..} -> fromMaybe (xs, [], vars) $ do
        left <- getValue siLeft
        right <- getValue siRight
        return $ case evalBinOp siBinOp left right of
          Nothing -> (xs, ssaNext, Map.insert siDest Bot vars)
          Just x -> (xs, ssaNext, Map.insert siDest x vars)

      -- Evaluate an unary operation.
      node@SUnOp{..} -> fromMaybe (xs, [], vars) $ do
        arg <- getValue siArg
        return $ case evalUnOp siUnOp arg of
          Nothing -> (xs, ssaNext, Map.insert siDest Bot vars)
          Just x -> (xs, ssaNext, Map.insert siDest x vars)

      -- Results of function calls are always variable.
      node@SCall{..} ->
        ( xs
        , []
        , foldl (\mp x -> Map.insert x Bot mp) vars siRet
        )

      -- Returns do nothing.
      node@SReturn{..} ->
        ( xs, [], vars )

      -- Constants are marked and propagated.
      node@SInt{..} ->
        (xs, ssaNext, Map.insert siDest (CInt siInt) vars)
      node@SBool{..} ->
        (xs, ssaNext, Map.insert siDest (CBool siBool) vars)
      node@SChar{..} ->
        (xs, ssaNext, Map.insert siDest (CChar siChar) vars)
      node@SCharArray{..} ->
        (xs, ssaNext, Map.insert siDest (CArray (Just (siDest, length siChars))) vars)


      -- | Evaluate conditionals.
      node@SBinJump{..} -> fromMaybe (cfgNext, [], vars) $ do
        left <- getValue siLeft
        right <- getValue siRight
        expr <- compareValue siCond left right
        if expr
          then do
            return ([(end, siWhere)], [], vars)
          else do
            return ([(end, fromJust $ Map.lookup end next)], [], vars)
      node@SUnJump{..} -> fromMaybe (cfgNext, [], vars) $ do
        expr <- getValue siArg
        case expr of
          CBool x | x == siWhen ->
            return ([(end, siWhere)], [], vars)
          CBool x | x /= siWhen ->
            return ([(end, fromJust $ Map.lookup end next)], [], vars)
          _ ->
            Nothing

      -- Add the unique branch target to the cfg worklist.
      node@SJump{..} -> ( xs, [], vars )
      where
        ssaNext = zip (repeat end) $ fromMaybe [] (Map.lookup end ssa)
        cfgNext = zip (repeat end) $ fromMaybe [] (Map.lookup end cfg)

        -- Looks up a value in the variable mapping.
        getValue var
          = Map.lookup var vars >>= \case
            Top -> Nothing
            x -> Just x
    -}

    -- Checks if any edges going to any other than (start, end) were marked
    -- as executed previously.
    anyOtherExecuted mark start end = fromMaybe False $ do
      nodes <- Map.lookup end cfg'
      return
        . any (\x -> x /= (start, end) && Set.member x mark)
        . zip nodes
        $ repeat end

    -- Checks if any of the edges entering the node were executed or not.
    anyExecuted mark end = fromMaybe False $ do
      nodes <- Map.lookup end cfg'
      return
        . any (\x -> Set.member (x, end) mark)
        $ nodes
