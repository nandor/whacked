{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.SCCP where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types



import Debug.Trace


data Value
  = Top
  | Bot
  | ConstInt Int
  deriving ( Eq, Ord, Show )


instance Monoid Value where
  mappend Top x = x
  mappend x Top = x
  mappend x y
    | x == y = x
    | otherwise = Bot

  mempty
    = Top


instance Num Value where
  -- addition
  ConstInt x + ConstInt y = ConstInt (x + y)
  Bot + _ = Bot
  _ + Bot = Bot

  -- multiplication
  ConstInt x * ConstInt y = ConstInt (x * y)
  ConstInt 0 * _ = ConstInt 0
  _ * ConstInt 0 = ConstInt 0
  Bot * _ = Bot
  _ * Bot = Bot

  -- subtraction
  ConstInt x - ConstInt y = ConstInt (x - y)
  Bot - _ = Bot
  _ - Bot = Bot

  -- Signum
  signum (ConstInt x) = ConstInt (signum x)
  signum x = x

  -- abs
  abs (ConstInt x) = ConstInt (abs x)
  abs x = x

  -- creation from an integer
  fromInteger = ConstInt . fromInteger


-- | Compares two values.
compareValue :: CondOp -> Value -> Value -> Maybe Bool
compareValue _ Bot _
  = Nothing
compareValue _ _ Bot
  = Nothing
compareValue op (ConstInt x) (ConstInt y)
  = Just $ (getComparator op) x y


-- | Builds the control flow graph.
buildCFGGraph :: [(Int, SInstr)] -> (Map Int [Int], Map Int [Int])
buildCFGGraph block
  = (cfg, cfg')
  where
    cfg = Map.fromList . zip (map fst block) . map build $ block
    cfg' = Map.foldlWithKey rev Map.empty cfg

    (count, _) = last block

    build (idx, SReturn{..})
      = []
    build (idx, SJump{..})
      = [siWhere]
    build (idx, SBinJump{..})
      | idx < count = [siWhere, idx + 1]
      | otherwise = [siWhere]
    build (idx, SUnJump{..})
      | idx < count = [siWhere, idx + 1]
      | otherwise = [siWhere]
    build (idx, _)
      | idx < count = [idx + 1]
      | otherwise = []

    rev cfg' node out
      = foldl (\cfg' x -> Map.insertWith (++) x [node] cfg') cfg' out


-- | Builds the SSA graph.
buildSSAGraph :: [(Int, SInstr)] -> ([SVar], Map Int [Int])
buildSSAGraph block
  = (map fst . Map.toList $ defs, foldl linkDefs Map.empty $ block)
  where
    defs = foldl findDefs Map.empty block

    findDefs mp (idx, SBinOp{..})
      = Map.insert siDest idx mp
    findDefs mp (idx, SConstInt{..})
      = Map.insert siDest idx mp
    findDefs mp (idx, SPhi{..})
      = foldl (\mp SPhiVar{..} -> Map.insert spVar idx mp) mp siVars
    findDefs mp _
      = mp

    linkDefs mp (idx, SBinOp{..})
      = foldl (linkTo idx) mp [siLeft, siRight]
    linkDefs mp (idx, SCall{..})
      = foldl (linkTo idx) mp siArgs
    linkDefs mp (idx, SPhi{..})
      = foldl (linkTo idx) mp (concatMap (map snd . spMerge) siVars)
    linkDefs mp (idx, SReturn{..})
      = foldl (linkTo idx) mp [siVal]
    linkDefs mp (idx, SBinJump{..})
      = foldl (linkTo idx) mp [siLeft, siRight]
    linkDefs mp (idx, SUnJump{..})
      = foldl (linkTo idx) mp [siVal]
    linkDefs mp _
      = mp

    linkTo idx mp var
      = case Map.lookup var defs of
        Nothing -> mp
        Just from -> Map.insertWith (++) from [idx] mp


optimise :: SFunction -> SFunction
optimise SFunction{..}
  = traceShow (mark, vars') . SFunction $ sfBody
  where
    (cfg, cfg') = buildCFGGraph sfBody
    (vars, ssa) = buildSSAGraph sfBody
    code = Map.fromList sfBody
    next = Map.fromList $ zip (map fst sfBody) (tail . map fst $ sfBody)

    (start, _) = Map.findMin cfg
    (mark, vars') = traverse
      [(start, start)]
      []
      Set.empty
      (Map.fromList . zip vars . repeat $ Top)


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
            let (xs', ys', var') = traverse' node [] edge mark vars
            in traverse xs' (ys' ++ ys) mark vars

    traverse [] [] mark vars
      = (mark, vars)

    traverse' node xs (start, end) mark vars = case node of
      -- Evaluate a phi node. When evaluating a phi node, we take into
      -- consideration the edges that come into the node and are marked
      -- executable. The meet function is then applied over those values.
      node@SPhi{..} ->
        let updatePhi (xs, ys, vars) SPhiVar{..}
              | (fromJust $ Map.lookup spVar vars) == newValue = (xs, ys, vars)
              | otherwise =
                  ( xs
                  , ys ++ ssaNext
                  , Map.insert spVar newValue vars
                  )
              where
                newValue = mconcat
                  [fromMaybe Top (Map.lookup x vars)
                  | (_, x) <- spMerge]
        in foldl updatePhi (xs, [], vars) siVars

      -- Evaluate a binary operation.
      node@SBinOp{..} -> fromMaybe (xs, [], vars) $ do
        left <- getValue siLeft
        right <- getValue siRight
        return $ case siBinOp of
          Add -> (xs, ssaNext, Map.insert siDest (left + right) vars)

      -- Results of function calls are always variable.
      node@SCall{..} ->
        ( xs
        , []
        , Map.insert siDest Top vars
        )

      -- Constants are marked and propagated.
      node@SConstInt{..} ->
        ( xs
        , ssaNext
        , Map.insert siDest (ConstInt siIntVal) vars
        )

      -- | Evaluate conditionals.
      node@SBinJump{..} -> fromMaybe (cfgNext, [], vars) $ do
        left <- getValue siLeft
        right <- getValue siRight
        case compareValue siCond left right of
          Nothing ->
            return (cfgNext, [], vars)
          Just x | x == siWhen ->
            return ([(end, siWhere)], [], vars)
          Just x | x /= siWhen ->
            return ([(end, fromJust $ Map.lookup end next)], [], vars)

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

    anyOtherExecuted mark start end
      = case Map.lookup end cfg' of
        Nothing -> False
        Just nodes ->
          let marked x = x /= (start, end) && Set.member x mark
          in any marked . zip nodes $ repeat end

    anyExecuted mark end
      = case Map.lookup end cfg' of
        Nothing -> False
        Just nodes -> any (\x -> Set.member (x, end) mark) nodes


sccp :: SProgram -> SProgram
sccp SProgram{..}
  = SProgram $ map optimise spFuncs