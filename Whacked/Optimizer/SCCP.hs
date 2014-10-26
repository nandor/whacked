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
  | CArray (Maybe (SVar, Int))
  | CPair (Maybe SVar)
  deriving ( Eq, Ord, Show )


-- |The monoid instance is used when aggregating values that come from
-- different PHI nodes. Top values are ignored, disjoint values yield Bot.
instance Monoid Value where
  mappend Top x = x
  mappend x Top = x
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
compareValue op x'@(CPair _) y'@(CPair _)
  = Just $ compareOp op x' y'
compareValue op x'@(CArray _) y'@(CArray _)
  = Just $ compareOp op x' y'


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
evalBinOp op x'@(CPair _) y'@(CPair _)
  = Just $ case op of
    Cmp CEQ -> CBool $ x' == y'
    Cmp CNE -> CBool $ x' /= y'
evalBinOp op x'@(CArray _) y'@(CArray _)
  = Just $ case op of
    Cmp CEQ -> CBool $ x' == y'
    Cmp CNE -> CBool $ x' /= y'


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
buildSSAGraph :: SFunction -> ([SVar], Map Int [Int])
buildSSAGraph func@SFunction{..}
  = (map fst . Map.toList $ defs, Map.mapWithKey (\k v -> nub v \\ [k]) ssa)
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
    ssa = Map.foldlWithKey linkFunc Map.empty sfBlocks
    linkFunc mp idx SBlock{ sbInstrs, sbPhis }
      = foldl (linkDefs idx) mp
      . concat
      $ (map spMerge sbPhis) ++ (map getGen sbInstrs)
    linkDefs i mp var
      = case Map.lookup var defs of
          Nothing -> mp
          Just def -> Map.insertWith (++) def [i] mp


-- |Performs sparse conditional constant propagation.
sccp :: SFunction -> SFunction
sccp func@SFunction{..}
  = traceShow (func, ssa) $ func
  where
    (cfg, cfg') = buildFlowGraph func
    (vars, ssa) = buildSSAGraph func
    (start, _) = Map.findMin sfBlocks
    {-

    (mark, vars') = traverse
      [(start, start)]
      []
      Set.empty
      (Map.fromList . zip vars . repeat $ Top)
    mark' = Set.map snd mark

    traverse (edge@(start, end):xs) ys mark vars
      = case Map.lookup end code of
        -- If the edge was already executed, stop propagation and continue
        -- with other avilable edges.
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

    anyOtherExecuted mark start end = fromMaybe False $ do
      nodes <- Map.lookup end cfg'
      let marked x = x /= (start, end) && Set.member x mark
      return $ any marked . zip nodes $ repeat end

    anyExecuted mark end = fromMaybe False $ do
      nodes <- Map.lookup end cfg'
      return $ any (\x -> Set.member (x, end) mark) nodes
    -}