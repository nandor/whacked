{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.SCCP where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.List (nub)
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Scratch
import           Whacked.Types
import           Whacked.FlowGraph



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

-- | Evaluates an unary operation.
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


-- | Builds the SSA graph.
buildSSAGraph :: [(Int, SInstr)] -> ([SVar], Map Int [Int])
buildSSAGraph block
  = (map fst . Map.toList $ defs, foldl linkDefs Map.empty $ block)
  where
    defs = foldl findDefs Map.empty block

    findDefs mp (idx, instr)
      = foldl (\mp x -> Map.insert x idx mp) mp (getKill instr)
    linkDefs mp (idx, instr)
      = foldl (linkTo idx) mp (getGen instr)

    linkTo idx mp var
      = case Map.lookup var defs of
        Nothing -> mp
        Just from -> Map.insertWith (++) from [idx] mp


optimise :: SFunction -> SFunction
optimise func@SFunction{..}
  = relabel func
    { sfBody = [(i, x) | (i, x) <- body', i `Set.member` reachable]
    }
  where
    bodyReduced = Map.fromList body'
    (body', varDecl, alias) = reduce sfBody Map.empty Map.empty
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

    -- Removes all expressions that are not used. An expression is used if
    -- in the SSA graph there is a path from it to a jump, call or return
    -- instructions.
    reachable = dfs [i | (i, x) <- body', isRoot x] Set.empty
    isRoot SBinJump{} = True
    isRoot SReturn{} = True
    isRoot SUnJump{} = True
    isRoot SCall{} = True
    isRoot SJump{} = True
    isRoot _ = False

    dfs (x:xs) visited
      |  x `Set.member` visited
        = dfs xs visited
      | Just instr <- Map.lookup x bodyReduced
        = dfs (findDecl (getGen instr) ++ xs) (Set.insert x visited)
      | otherwise
        = dfs xs visited
      where
        findDecl vars
          = concatMap (\x -> fromMaybe [] ((:[]) <$> Map.lookup x varDecl)) vars
    dfs [] visited
      = visited

    -- Replaces computable binary expressions with constants and removes
    -- redundant jump instructions.
    reduce ((i, instr):ns) vars alias
      | not (i `Set.member` mark') = reduce ns vars alias
      | otherwise = case instr of
        jmp@SBinJump{} ->
          let jmp' = fromMaybe jmp
                      { siLeft = findAlias $ siLeft jmp
                      , siRight = findAlias $ siRight jmp
                      } $ do
                        left <- evalValue (findAlias . siLeft $ jmp)
                        right <- evalValue (findAlias . siRight $ jmp)
                        val <- compareValue (siCond jmp)  left right
                        return $ if val == siWhen jmp
                            then SJump (siWhere jmp)
                            else SJump . fromJust . Map.lookup i $ next
          in case ns' of
            next@(x, _) : ns' | i <= siWhere jmp' && siWhere jmp' <= x ->
              (next:ns', vars', alias')
            ns' ->
              ((i, jmp') : ns', vars', alias')
        jmp@SUnJump{} ->
          let jmp' = fromMaybe jmp{ siArg = findAlias $ siArg jmp } $ do
                    val <- evalValue (findAlias . siArg $ jmp)
                    case val of
                      CBool x | x == siWhen jmp ->
                        return $ SJump (siWhere jmp)
                      CBool x | x /= siWhen jmp ->
                        return $ SJump . fromJust . Map.lookup i $ next
          in case ns' of
            next@(x, _) : ns' | i <= siWhere jmp' && siWhere jmp' <= x ->
              (next:ns', vars', alias')
            ns' ->
              ((i, jmp') : ns', vars', alias')
        jmp@SJump{..} ->
          case ns' of
            next@(x, _) : ns' | x >= siWhere -> (next:ns', vars', alias')
            ns' -> ((i, jmp) : ns', vars', alias')
        phi@SPhi{..} ->
          let phi' = fromMaybe phi
                      { siMerge
                          = nub
                          . filter ((`Map.member` vars))
                          . map findAlias
                          $ siMerge
                      } $ do
                        result <- mconcat <$> mapM evalValue siMerge
                        case result of
                          CInt x -> return $ SInt siDest x
                          _ -> Nothing
          in case phi' of
            SPhi{ siMerge = [x], siDest } ->
              reduce ns vars (Map.insert siDest (findAlias x) alias)
            _ -> ((i, phi') : ns', vars', alias')
        bin@SBinOp{..} ->
          let bin' = fromMaybe bin
                      { siLeft = findAlias siLeft
                      , siRight = findAlias siRight
                      , siDest = findAlias siDest
                      } $ do
                    left <- evalValue (findAlias siLeft)
                    right <- evalValue (findAlias siRight)
                    result <- evalBinOp siBinOp left right
                    case result of
                      CInt x -> return $ SInt (findAlias siDest) x
                      CBool x -> return $ SBool (findAlias siDest) x
                      _ -> Nothing
          in ( (i, bin') : ns'
             , vars'
             , alias'
             )
        un@SUnOp{..} ->
          let un' = fromMaybe un $ do
                   arg <- evalValue (findAlias siArg)
                   result <- evalUnOp siUnOp arg
                   case result of
                      CInt x -> return $ SInt siDest x
                      CBool x -> return $ SBool siDest x
                      _ -> Nothing
          in ( (i, un') : ns'
             , vars'
             , alias'
             )

        call@SCall{..} ->
          ( ( i
            , call
              { siArgs = map findAlias siArgs
              , siRet = map findAlias siRet
              }
            ) : ns'
          , vars'
          , alias'
          )
        ret@SReturn{..} ->
          ((i, ret{ siArg = findAlias siArg }) : ns', vars', alias')
        _ -> ((i, instr):ns', vars', alias')
      where
        findAlias var
          = fromMaybe var . Map.lookup var $ alias
        (ns', vars', alias')
          = reduce ns (foldr (\x -> Map.insert x i) vars (getKill instr)) alias
    reduce [] vars alias
      = ([], vars, alias)

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
