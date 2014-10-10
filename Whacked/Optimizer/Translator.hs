{-# LANGUAGE GeneralizedNewtypeDeriving,
             LambdaCase,
             NamedFieldPuns,
             RecordWildCards #-}
module Whacked.Optimizer.Translator
  ( generateS
  ) where

import           Control.Applicative
import           Control.Monad
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



-- | Control flow graph structure.
type FlowGraph
  = Map Int (Set Int)


-- | First step is to group statements into blocks. Blocks start with
-- labels and end with jump statements or normal statements. groups will be
-- a map of individual blocks, while target will map label indices to the
-- blocks they are in.
getBlocks :: [IInstr]
          -> (Map Int [IInstr], Map Int Int)
getBlocks body
  = (blocks, target)
  where
    (blocks, target, _, _) = group Map.empty Map.empty [] 0 body
    group blocks target block next (instr:instrs) = case instr of
      ILabel{..} | block == [] ->
        group
          blocks
          (Map.insert iiIndex next target)
          [instr]
          next
          instrs
      ILabel{..} ->
        group
          (Map.insert next (reverse block) blocks)
          (Map.insert iiIndex (next + 1) target)
          [instr]
          (next + 1)
          instrs
      IReturn{..} -> group'
      IBinJump{..} -> group'
      IUnJump{..} -> group'
      IJump{..} -> group'
      _ -> group blocks target (instr:block) next instrs
      where
        group'
          = group
            (Map.insert next (reverse $ instr:block) blocks)
            target
            []
            (next + 1)
            instrs
    group blocks target block next []
      | block == [] =
        ( blocks
        , target
        , block
        , next
        )
      | otherwise =
          ( Map.insert next (reverse block) blocks
          , target
          , []
          , next + 1
          )


-- | Build up the flow graph and the reverse flow graph which is going to
-- be used to compute dominance frontiers.
getGraph :: Map Int [IInstr]
         -> Map Int Int
         -> (FlowGraph, FlowGraph)
getGraph blocks target
  = (graph, graph')
  where
    graph = Map.mapWithKey forwardLink blocks
    forwardLink index []
      = Set.empty
    forwardLink index group
      = Set.filter (`Map.member` blocks) $ case last group of
        IJump{..} -> Set.singleton (fromJust . Map.lookup iiWhere $ target)
        IBinJump{..} -> Set.fromList
          [ fromJust . Map.lookup iiWhere $ target
          , index + 1
          ]
        IUnJump{..} -> Set.fromList
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


-- | Computes the set of dominators of a node. This function is based on the
-- paper "A Simple, Fast Dominance Algorithm" by Keith D. Cooper,
-- Timothy J. Harvey, and Ken Kennedy. Its running time is O(N^2).
-- If you have any doubts about this functions, RTFM.
-- This function runs in the ST monad for performance reasons.
getDominators :: FlowGraph
              -> FlowGraph
              -> Vector Int
getDominators graph graph' = runST $ do
  let (count, _) = Map.findMax graph
  dom <- MVector.new (count + 1)

  MVector.write dom 0 (Just 0)
  forM_ [1..count] $ \i -> do
    MVector.write dom i Nothing

  let findDominators = do
        changed <- forM [0..count] $ \i -> case Map.lookup i graph' of
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
        when (or changed) findDominators

      intersect x y
        | x == y = return x
        | x > y = do
          x' <- fromJust <$> MVector.read dom x
          intersect x' y
        | x < y = do
          y' <- fromJust <$> MVector.read dom y
          intersect x y'

  findDominators
  dom <- forM [0..count] $ (MVector.read dom)
  return (Vector.fromList . map fromJust $ dom)


-- | Computes the dominance frontier. The dominance frontier of a node is
-- the list of nodes that are not directly dominated by it.
getFrontier :: FlowGraph
            -> FlowGraph
            -> Vector Int
            -> Map Int (Set Int)
getFrontier graph graph' dominators
  = foldl findFrontier Map.empty [0..count]
  where
    (count, _) = Map.findMax graph
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


-- | For each block, computes which variables require PHI nodes. Starting from
-- blocks that define a variable, the variable is propagated into the phi sets
-- of the nodes that are at the dominance frontier.
getPhiNodes :: Set (String, Int, Type)
            -> Map Int [IInstr]
            -> Map Int (Set Int)
            -> Map Int [(String, Int, Type)]
getPhiNodes vars blocks frontier
  = foldl findPhiNodes Map.empty . Set.toList $ vars
  where
    findPhiNodes mp var@(name, scope, t)
      = visits mp queue Set.empty Set.empty
      where
        queue = map fst . Map.toList . Map.filter (any assignsTo) $ blocks
        assignsTo IWriteVar{..}
          = iiVar == name
        assignsTo _
          = False

        visits mp [] inserted added
          = mp
        visits mp (q:qs) inserted added
          = case Set.toList <$> Map.lookup q frontier of
              Just dom ->
                let insert (inserted, added, mp, qs) node
                      | Set.member node inserted = (inserted, added, mp, qs)
                      | otherwise =
                        ( Set.insert node inserted
                        , Set.insert node added
                        , Map.insertWith (++) node [var] mp
                        , if Set.member node added then qs else node : qs
                        )
                    (inserted', added', mp', qs')
                      = foldl insert (inserted, added, mp, qs) dom
                in visits mp' qs' inserted' added'
              Nothing -> visits mp qs inserted added


-- | Scope used to track the next unique instruction labels and version numbers
-- for variables.
data Scope
  = Scope
    { nextTemp  :: Int
    , nextInstr :: Int
    , block     :: Int
    , vars      :: Map (String, Int) (Type, SVar)
    , vars'     :: Map SVar (String, Int)
    }
  deriving ( Eq, Ord, Show )


-- |Monads stack out of a state and a writer.
newtype Generator a
  = Generator { run :: StateT Scope (Writer [(Int, SInstr)]) a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadState Scope
           , MonadWriter [(Int, SInstr)]
           )

-- | Runs the generator monad.
runGenerator :: Generator a -> (a, Scope, [(Int, SInstr)])
runGenerator gen
  = let ((a, scope), instrs) = runWriter . runStateT (run gen) $ Scope
          { nextTemp = 0
          , nextInstr = 0
          , block = 0
          , vars = Map.empty
          , vars' = Map.empty
          }
    in (a, scope, instrs)


genTemp :: Generator SVar
genTemp = do
  scope@Scope{ nextTemp } <- get
  put scope{ nextTemp = nextTemp + 1 }
  return $ SVar nextTemp


emit :: SInstr -> Generator ()
emit instr = do
  scope@Scope{ nextInstr, block } <- get
  put scope{ nextInstr = nextInstr + 1 }
  tell [(block, instr)]


genExpr :: IExpr -> SVar -> Generator (Type, SVar)
genExpr IBinOp{..} dest = do
  (lt, le) <- genTemp >>= genExpr ieLeft
  (rt, re) <- genTemp >>= genExpr ieRight
  emit $ SBinOp lt dest ieBinOp le re
  return (lt, dest)
genExpr IVar{..} dest = do
  Scope{ vars } <- get
  return . fromJust $ Map.lookup (ieName, ieScope) vars
genExpr IConstInt{..} dest = do
  emit $ SConstInt dest ieIntVal
  return (Int, dest)

genExpr IUnOp{..} dest
  = undefined
genExpr ICall{..} dest
  = undefined
genExpr IConstReal{..} dest
  = undefined
genExpr IConstChar{..} dest
  = undefined
genExpr IConstString{..} dest
  = undefined


-- | Generates Scratchy intermediate code out of Itchy expressions.
genInstr :: IInstr -> Generator ()
genInstr ILabel{..} = do
  return ()

genInstr IReturn{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  emit $ SReturn t expr

genInstr IBinJump{..} = do
  (lt, le) <- genTemp >>= genExpr iiLeft
  (rt, re) <- genTemp >>= genExpr iiRight
  emit $ SBinJump lt iiWhere iiWhen iiCond le re

genInstr IJump{..} = do
  emit $ SJump iiWhere

genInstr IWriteVar{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  scope@Scope{ vars } <- get
  put scope{ vars = Map.insert (iiVar, iiScope) (t, expr) vars }

genInstr IPrint{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  let func = case t of
        Int  -> "__print_int"
  emit $ SCall Void SVoid func [expr]


-- | Generates code for a function.
genFunc :: IFunction
        -> SFunction
genFunc func@IFunction{..}
  = SFunction instrs'
  where
    (blocks, target) = getBlocks ifBody
    (graph, graph') = getGraph blocks target
    dominators = getDominators graph graph'
    frontier = getFrontier graph graph' dominators
    phi = getPhiNodes ifVars blocks frontier

    ((indices, vars'), _, instrs) = runGenerator $ do
      indices <- forM (Map.toList blocks) $ \(idx, instrs) -> do
        scope <- get
        let blockStart = nextInstr scope
        put scope{ block = idx}
        let phi' = Map.lookup idx phi
        when (phi' /= Nothing) $
          forM_ (fromJust phi') $ \(var, idx, t) -> do
            expr <- genTemp
            scope@Scope{ vars, vars' } <- get
            put scope
              { vars = Map.insert (var, idx) (t, expr) vars
              , vars' = Map.insert expr (var, idx) vars'
              }
            emit $ SPhi t expr []

        mapM_ genInstr instrs
        Scope{ vars } <- get
        return (idx, (blockStart, vars))
      Scope{ vars' } <- get
      return (indices, vars')

    target' = Map.fromList indices
    instrs' = zip [0..] $ map (fillPhi . relabel') instrs

    relabel' (block, jump@SBinJump{..})
      = (block, jump{ siWhere = lookupLabel siWhere})
    relabel' (block, jump@SUnJump{..})
      = (block, jump{ siWhere = lookupLabel siWhere})
    relabel' (block, jump@SJump{..})
      = (block, jump{ siWhere = lookupLabel siWhere})
    relabel' x
      = x

    fillPhi (block, phi@SPhi{..})
      = case Map.lookup siDest vars' of
          Nothing -> phi
          Just var -> phi{ siMerge = getVersions . map (findVar var) $ prev }
      where
        prev
          = fromMaybe [] (Set.toList <$> Map.lookup block graph')
        findVar var block = do
          (_, vars) <- Map.lookup block target'
          var' <- Map.lookup var vars
          return var'
        getVersions
          = map  (snd . fromJust ) . filter (/= Nothing)

    fillPhi (_, x)
      = x

    lookupLabel i
      = fromJust $ do
        block <- Map.lookup i target
        (instr, _) <- Map.lookup block target'
        return instr


-- | Generates unoptimised Scratchy code for a program.
generateS :: IProgram
          -> SProgram
generateS IProgram{..}
  = SProgram (map genFunc ipFuncs)