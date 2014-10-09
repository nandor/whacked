{-# LANGUAGE GeneralizedNewtypeDeriving,
             LambdaCase,
             NamedFieldPuns,
             RecordWildCards #-}
module Whacked.Optimizer.Generator
  ( generateS
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.ST
import           Control.Monad.Writer
import           Data.List ((\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.STRef
import           Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import           Whacked.Itch
import           Whacked.Scratch
import           Whacked.Types


import Debug.Trace


-- |The scope keeps information about declared variables, functions return
-- types, label counters and scope counters. It is wrapped into a state monad
-- for easy access.
data Scope
  = Scope
    { function  :: String
    , functions :: Map String IFunction
    , nextScope :: Int
    , nextLabel :: Int
    , variables :: [(Int, Map String Type)]
    }
  deriving ( Eq, Ord, Show )


-- |The generator monad obtained by stacking a state and a writer.
newtype Generator a
  = Generator (StateT Scope (Writer [IInstr]) a)
  deriving ( Applicative
           , Functor
           , Monad
           , MonadState Scope
           , MonadWriter [IInstr]
           )


genFunc :: IFunction -> SFunction
genFunc IFunction{..}
  = traceShow (frontier) SFunction
  where
    -- First step is to group statements into blocks. Blocks start with
    -- labels and end with jump statements or normal statements. groups will be
    -- a map of individual blocks, while target will map label indices to the
    -- blocks they are in.
    (groups, target, _, count) = group Map.empty Map.empty [] 0 ifBody

    group :: Map Int [IInstr] -> Map Int Int -> [IInstr] -> Int -> [IInstr] ->
      (Map Int [IInstr], Map Int Int, [IInstr], Int)
    group groups target block next (instr:instrs) = case instr of
      ILabel{..} | block == [] ->
        group
          groups
          (Map.insert iiIndex next target)
          [instr]
          next
          instrs
      ILabel{..} ->
        group
          (Map.insert next (reverse block) groups)
          (Map.insert iiIndex (next + 1) target)
          [instr]
          (next + 1)
          instrs
      IReturn{..} -> group'
      IJump{..} -> group'
      IUJump{..} -> group'
      _ -> group groups target (instr:block) next instrs
      where
        group'
          = group
            (Map.insert next (reverse $ instr:block) groups)
            target
            []
            (next + 1)
            instrs
    group groups target block next []
      | block == [] =
        ( groups
        , target
        , block
        , next
        )
      | otherwise =
          ( Map.insert next (reverse block) groups
          , target
          , []
          , next + 1
          )

    -- Build up the flow graph and the reverse flow graph which is going to
    -- be used to compute dominance frontiers.
    graph = Map.mapWithKey forwardLink groups
    forwardLink index []
      = Set.empty
    forwardLink index group = Set.filter (< count) $ case last group of
        IUJump{..} -> Set.singleton (fromJust . Map.lookup iiWhere $ target)
        IJump{..} -> Set.fromList
          [ fromJust . Map.lookup iiWhere $ target
          , index + 1
          ]
        IReturn{..} -> Set.empty
        _ -> Set.singleton (index + 1)
    graph' = Map.foldlWithKey backLink Map.empty graph
    backLink graph index out
      = Set.foldl backLink' graph out
      where
        backLink' graph out'
          = case Map.lookup out' graph of
            Nothing -> Map.insert out' (Set.singleton index) graph
            Just s -> Map.insert out' (Set.insert index s) graph

    -- Computes the reverse postorder by doing a reverse topological sort.
    -- The reverse postorder has the property that all nodes are visited before
    -- their successors, except the cases when they are reaching through
    -- backwards edges.
    (order, _) = dfs 0 [] Set.empty
    index = Vector.fromList order
    dfs node acc viz
      | Set.member node viz = (acc, viz)
      | Just neighbours <- Map.lookup node graph =
        let (acc', viz') = Set.foldl next (acc, Set.insert node viz) neighbours
        in (node:acc', viz')
      | otherwise = (acc, viz)
      where
        next (acc, viz) node
          = dfs node acc viz

    -- Compute the set of dominators of a node. This function is based on the
    -- paper "A Simple, Fast Dominance Algorithm" by Keith D. Cooper,
    -- Timothy J. Harvey, and Ken Kennedy. Its running time is O(N^2).
    -- If you have any doubts about this functions, RTFM.
    dominators
      = runST $ do
          dom <- MVector.new count

          MVector.write dom 0 (Just 0)
          forM_ [1..count - 1] $ \i -> do
            MVector.write dom i Nothing

          let findDominators = do
                changed <- forM order $ \i -> case Map.lookup i graph' of
                  Nothing -> return False
                  Just prev -> do
                    let prevl = Set.toList prev
                    prev' <- zip prevl <$> mapM (MVector.read dom) prevl
                    case dropWhile (isNothing . snd) prev' of
                      [] -> return False
                      (first, _):_ -> do
                        idom' <- foldM
                          (\idom p -> MVector.read dom p >>= \case
                              Nothing -> return idom
                              Just _ -> intersect p idom)
                          first (prevl \\ [first])

                        previousDom <- MVector.read dom i
                        if Just idom' /= previousDom
                          then do
                            MVector.write dom i (Just idom')
                            return True
                          else return False
                return (or changed)

              intersect x y
                = case xi `compare` yi of
                    EQ -> return x
                    GT -> do
                      x' <- fromJust <$> MVector.read dom x
                      intersect x' y
                    LT -> do
                      y' <- fromJust <$> MVector.read dom y
                      intersect x y'
                where
                  xi = index ! x
                  yi = index ! y

              findDominators' = do
                changed <- findDominators
                when changed $ findDominators'

          findDominators'
          dom <- forM [0..count - 1] $ (MVector.read dom)
          return (Vector.fromList . map fromJust $ dom)

    -- Computes the dominance frontier.
    frontier = foldl findFrontier Map.empty [0..count - 1]
    findFrontier ms node
      = case Map.lookup node graph' of
          Just prev | Set.size prev > 1 -> Set.foldl run ms prev
          _ -> Map.insert node Set.empty ms
      where
        run ms runner
          | runner == dominators ! node = ms
          | otherwise = case Map.lookup runner ms of
            Nothing -> run (Map.insert runner (Set.singleton node) ms) next
            Just set -> run (Map.insert runner (Set.insert node set) ms) next
          where
            next = dominators ! runner


generateS :: IProgram -> SProgram
generateS IProgram{..}
  = SProgram (map genFunc ipFuncs)