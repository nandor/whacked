{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.SCCP where

import           Control.Applicative
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types
import           Whacked.FlowGraph



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

-- | Evaluates an operation.
evalBinOp :: BinaryOp -> Value -> Value -> Maybe Value
evalBinOp _ Bot _
  = Nothing
evalBinOp _ _ Bot
  = Nothing
evalBinOp op (ConstInt x) (ConstInt y)
  = return $ case op of
    Add -> ConstInt $ x + y


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
      = Map.insert siDest idx mp
    findDefs mp _
      = mp

    linkDefs mp (idx, SBinOp{..})
      = foldl (linkTo idx) mp [siLeft, siRight]
    linkDefs mp (idx, SCall{..})
      = foldl (linkTo idx) mp siArgs
    linkDefs mp (idx, SPhi{..})
      = foldl (linkTo idx) mp siMerge
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
optimise func@SFunction{..}
  = func{ sfBody = [(i, reduce i x) | (i, x) <- sfBody, Set.member i mark' ] }
  where
    (cfg, cfg') = buildFlowGraph sfBody
    (vars, ssa) = buildSSAGraph sfBody
    code = Map.fromList sfBody
    next = Map.fromList $ zip (map fst sfBody) (tail . map fst $ sfBody)

    (start, _) = Map.findMin cfg
    (mark, vars') = traverse
      [(start, start)]
      []
      Set.empty
      (Map.fromList . zip vars . repeat $ Top)
    mark' = Set.map snd mark

    reduce node jmp@SBinJump{..} = fromMaybe jmp $ do
      left <- evalValue siLeft
      right <- evalValue siRight
      return $ case compareValue siCond left right of
        Nothing -> jmp
        Just x | x == siWhen -> SJump siWhere
        _ -> SJump . fromJust . Map.lookup node $ next

    reduce node phi@SPhi{..} = fromMaybe phi $ do
      result <- mconcat <$> mapM evalValue siMerge
      case result of
        ConstInt x -> return $ SConstInt siDest x
        _ -> Nothing

    reduce node bin@SBinOp{..} = fromMaybe bin $ do
      left <- evalValue siLeft
      right <- evalValue siRight
      result <- evalBinOp siBinOp left right
      case result of
        ConstInt x -> return $ SConstInt siDest x
        _ -> Nothing

    reduce node x
      = x

    -- Used in the reduction step to evaluate a value.
    evalValue var
      = case Map.lookup var vars' of
          Just Bot -> Nothing
          x -> x

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
        return $ case siBinOp of
          Add -> (xs, ssaNext, Map.insert siDest (left + right) vars)

      -- Results of function calls are always variable.
      node@SCall{..} ->
        ( xs
        , []
        , Map.insert siDest Bot vars
        )

      -- Returns do nothing.
      node@SReturn{..} ->
        ( xs, [], vars)

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