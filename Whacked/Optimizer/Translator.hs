{-# LANGUAGE GeneralizedNewtypeDeriving,
             LambdaCase,
             NamedFieldPuns,
             RecordWildCards,
             TupleSections #-}
module Whacked.Optimizer.Translator
  ( generateS
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Char
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

-- | Control flow graph structure.
type FlowGraph
  = Map Int (Set Int)


-- | First step is to group statements into blocks. Blocks start with
-- labels and end with jump statements or normal statements. groups will be
-- a map of individual blocks, while target will map label indices to the
-- blocks they are in.
getBlocks :: [IInstr] -> (Map Int [IInstr], Map Int Int)
getBlocks body
  = (blocks, target)
  where
    (blocks, target, _, _) = group Map.empty Map.empty [] 0 body
    group blocks target block next (instr:instrs) = case instr of
      ILabel{..} | null block ->
        group
          blocks
          (Map.insert iiLabel next target)
          [instr]
          next
          instrs
      ILabel{..} ->
        group
          (Map.insert next (reverse block) blocks)
          (Map.insert iiLabel (next + 1) target)
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
      | null block =
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
getGraph :: Map Int [IInstr] -> Map Int Int -> (FlowGraph, FlowGraph)
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
    backLink graph index
      = Set.foldl backLink' graph
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
getDominators :: FlowGraph -> FlowGraph -> Vector (Maybe Int)
getDominators graph graph' = runST $ do
  let (count, _) = Map.findMax graph
  dom <- MVector.new (count + 1)

  MVector.write dom 0 (Just 0)
  forM_ [1..count] $ \i -> MVector.write dom i Nothing

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
          x' `intersect` y
        | x < y = do
          y' <- fromJust <$> MVector.read dom y
          x `intersect` y'

  findDominators
  Vector.fromList <$> forM [0..count] (MVector.read dom)


-- | Computes the dominance frontier. The dominance frontier of a node is
-- the list of nodes that are not directly dominated by it.
getFrontier :: FlowGraph -> FlowGraph -> Vector (Maybe Int) -> Map Int (Set Int)
getFrontier graph graph' dominators
  = foldl findFrontier Map.empty [0..count]
  where
    (count, _) = Map.findMax graph
    findFrontier ms node
      = case Map.lookup node graph' of
          Just prev | Set.size prev > 1 -> Set.foldl run ms prev
          _ -> Map.insert node Set.empty ms
      where
        run ms runner = case dominators ! node of
          Just runner' | runner' /= runner -> fromMaybe ms $ do
            next <- dominators ! runner
            return $ run (Map.insert runner set' ms) next
          _ -> ms
          where
            set' = Set.insert node (fromMaybe Set.empty $ Map.lookup runner ms)


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
        assignsTo IAssVar{..}
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
    { nextTemp :: Int
    , block    :: Int
    , vars     :: Map (String, Int) (Type, SVar)
    , vars'    :: Map SVar (String, Int)
    , blockVar :: Map Int (Map (String, Int) (Type, SVar))
    , blocks   :: Map Int SBlock
    , labels   :: Map Int Int
    }
  deriving ( Eq, Ord, Show )


-- |Monads stack out of a state and a writer.
newtype Generator a
  = Generator { run :: State Scope a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadState Scope
           )

-- | Runs the generator monad.
runGenerator :: Map Int Int -> Generator a -> (a, Scope)
runGenerator labels gen
  = let (a, scope) = runState (run gen) Scope
          { nextTemp = 0
          , block = 0
          , vars = Map.empty
          , vars' = Map.empty
          , blocks = Map.empty
          , blockVar = Map.empty
          , labels = labels
          }
    in (a, scope)


genTemp :: Generator SVar
genTemp = do
  scope@Scope{ nextTemp } <- get
  put scope{ nextTemp = nextTemp + 1 }
  return $ SVar nextTemp


emit :: SInstr -> Generator ()
emit instr = do
  scope@Scope{ blocks, block } <- get
  put $ case Map.lookup block blocks of
    Nothing -> scope
      { blocks = Map.insert block (SBlock [] [ instr ]) blocks
      }
    Just sblock@SBlock{ sbInstrs } -> scope
      { blocks = Map.insert block sblock
        { sbInstrs = sbInstrs ++ [instr]
        } blocks
      }


getLabel :: Int -> Generator Int
getLabel idx = do
  Scope{ labels } <- get
  return $ fromJust (Map.lookup idx labels)


genExpr :: IExpr -> SVar -> Generator (Type, SVar)
genExpr IBinOp{..} dest = do
  (rt, re, lt, le) <-
    if isCommutative ieBinOp && height ieLeft < height ieRight
      then do
        (rt, re) <- genTemp >>= genExpr ieRight
        (lt, le) <- genTemp >>= genExpr ieLeft
        return (rt, re, lt, le)
      else do
        (lt, le) <- genTemp >>= genExpr ieLeft
        (rt, re) <- genTemp >>= genExpr ieRight
        return (rt, re, lt, le)
  emit $ SBinOp lt dest ieBinOp le re
  return (fromJust $ binOpType ieBinOp lt rt, dest)
genExpr IUnOp{..} dest = do
  (t, expr) <- genTemp >>= genExpr ieArg
  emit $ SUnOp t dest ieUnOp expr
  return (fromJust $ unOpType ieUnOp t, dest)
genExpr IVar{..} dest = do
  Scope{ vars } <- get
  return . fromJust $ Map.lookup (ieName, ieScope) vars
genExpr IBool{..} dest = do
  emit $ SBool dest ieBool
  return (Bool, dest)
genExpr IChar{..} dest = do
  emit $ SChar dest ieChar
  return (Char, dest)
genExpr IInt{..} dest = do
  emit $ SInt dest ieInt
  return (Int, dest)
genExpr IString{..} dest = do
  emit $ SString dest ieString
  return (String, dest)
genExpr IArray{ ieElems = [], ..} dest = do
  emit $ SNewArray Empty dest 0
  return (Empty, dest)
genExpr IArray{..} dest = do
  emit $ SNewArray ieType dest (length ieElems)
  forM_ (zip [0..] ieElems) $ \(i, elem) -> do
    (t, expr) <- genTemp >>= genExpr elem
    idx <- genTemp
    emit $ SInt idx i
    emit $ SWriteArray dest idx expr t
  return (Array ieType, dest)
genExpr IIndex{..} dest = do
  (Array t, arr) <- genTemp >>= genExpr ieArray
  emit $ SCheckNull arr
  (_, idx) <- genTemp >>= genExpr ieIndex
  emit $ SReadArray dest arr idx t
  return (t, dest)
genExpr IPair{..} dest = do
  (lt, lexpr) <- genTemp >>= genExpr ieFst
  (rt, rexpr) <- genTemp >>= genExpr ieSnd
  emit $ SNewPair dest
  emit $ SWritePair Fst dest lexpr
  emit $ SWritePair Snd dest rexpr
  return (Pair lt rt, dest)
genExpr IElem{..} dest = do
  (pt, pexpr) <- genTemp >>= genExpr iePair
  emit $ SCheckNull pexpr
  case pt of
    Pair lt rt | ieElem == Fst -> do
      emit $ SReadPair Fst dest pexpr
      return (lt, dest)
    Pair lt rt | ieElem == Snd -> do
      emit $ SReadPair Snd dest pexpr
      return (rt, dest)
    _ -> do
      emit $ SReadPair ieElem dest pexpr
      return (ieType, dest)
genExpr ICall{..} dest = do
  args <- mapM (\x -> genTemp >>= genExpr x) ieArgs
  dest <- genTemp
  emit $ SCall [dest] ieName (map snd args)
  return (ieType, dest)
genExpr IRead{..} dest = case ieType of
  Int -> do
    emit $ SCall [dest] "__read_int" []
    return (Int, dest)
  Char -> do
    emit $ SCall [dest] "__read_char" []
    return (Char, dest)
  Array Char -> do
    emit $ SCall [dest] "__read_string" []
    return (Array Char, dest)
genExpr INull{} dest = do
  emit $ SInt dest 0
  return (Null, dest)


-- | Generates Scratchy intermediate code out of Itchy expressions.
genInstr :: IInstr -> Generator ()
genInstr ILabel{..}
  = return ()
genInstr IReturn{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  emit $ SReturn expr
genInstr IExit{..} = do
  (_, expr) <- genTemp >>= genExpr iiExpr
  emit $ SCall [] "exit" [expr]
genInstr IBinJump{..} = do
  (lt, le) <- genTemp >>= genExpr iiLeft
  (rt, re) <- genTemp >>= genExpr iiRight
  labels <- getLabel iiWhere
  emit $ SBinJump labels iiCond le re
genInstr IUnJump{..} = do
  (t, expr) <- genTemp >>= genExpr iiVal
  labels <- getLabel iiWhere
  emit $ SUnJump labels iiWhen expr
genInstr IJump{..} = do
  labels <- getLabel iiWhere
  emit $ SJump labels
genInstr IAssVar{..} = do
  dest <- genTemp
  (t, expr) <- genExpr iiExpr dest
  when (dest /= expr) $
    emit $ SMov dest expr
  scope@Scope{ vars } <- get
  put scope{ vars = Map.insert (iiVar, iiScope) (t, dest) vars }
genInstr IPrint{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  case t of
    Int        -> emit $ SCall [] "__print_int" [expr]
    Char       -> emit $ SCall [] "__print_char" [expr]
    Bool       -> emit $ SCall [] "__print_bool" [expr]
    String     -> emit $ SCall [] "__print_string" [expr]
    Array Char -> emit $ SCall [] "__print_string" [expr]
    _          -> emit $ SCall [] "__print_ref" [expr]
genInstr IPrintln{..} = do
  genInstr (IPrint iiExpr)
  temp <- genTemp
  emit $ SChar temp '\n'
  emit $ SCall [] "__print_char" [temp]
genInstr IEnd{} = do
  expr <- genTemp
  emit $ SInt expr 0
  emit $ SReturn expr
genInstr IAssArray{..} = do
  (_, array) <- genTemp >>= genExpr iiArray
  emit $ SCheckNull array
  (_, idx)   <- genTemp >>= genExpr iiIndex
  (t, expr)  <- genTemp >>= genExpr iiExpr
  emit $ SWriteArray array idx expr t
genInstr IAssPair{..} = do
  (_, pair) <- genTemp >>= genExpr iiPair
  emit $ SCheckNull pair
  (t, expr)  <- genTemp >>= genExpr iiExpr
  emit $ SWritePair iiElem pair expr
genInstr IFree{..} = do
  (t, expr) <- genTemp >>= genExpr iiExpr
  emit $ SFree expr


-- | Generates code for a function.
genFunc :: IFunction -> SFunction
genFunc func@IFunction{..}
  = SFunction (Map.mapWithKey computePhi blocks') args ifName
  where
    (blocks, labels) = getBlocks ifBody
    (graph, graph') = getGraph blocks labels
    dominators = getDominators graph graph'
    frontier = getFrontier graph graph' dominators
    phi = getPhiNodes ifVars blocks frontier

    (args, scope@Scope{ blocks = blocks', blockVar }) = runGenerator labels $ do
      args <- forM ifArgs $ \(t, name) -> do
        expr <- genTemp
        scope@Scope{ vars, vars' } <- get
        put scope{ vars = Map.insert (name, 0) (t, expr) vars
                 , vars' = Map.insert expr (name, 0) vars'
                 }
        return expr

      forM_ (Map.toList blocks) $ \(idx, instrs) -> do
        -- Place empty phi nodes for all affected variables.
        case Map.lookup idx phi of
          Nothing -> do
            scope@Scope{ blocks, blockVar } <- get
            put scope
              { block = idx
              , blocks = Map.insert idx (SBlock [] []) blocks
              }
            case Set.toList <$> Map.lookup idx graph' of
              Nothing -> return ()
              Just xs -> get >>= \scope@Scope{vars} -> put scope
                  { vars = Map.unions . mapMaybe (`Map.lookup` blockVar) $ xs
                  }

            mapM_ genInstr instrs
          Just phi' -> do
            phis <- forM phi' $ \(var, idx, t) -> do
              expr <- genTemp
              scope@Scope{ vars, vars', blocks } <- get
              put scope
                { vars = Map.insert (var, idx) (t, expr) vars
                , vars' = Map.insert expr (var, idx) vars'
                }
              return $ SPhi expr t []

            get >>= \scope@Scope{ blocks } -> put scope
              { block = idx
              , blocks = Map.insert idx (SBlock phis []) blocks
              }

            mapM_ genInstr instrs

        -- Returns a snapshot of variable versions.
        scope@Scope{ vars, blockVar } <- get
        put scope{ blockVar = Map.insert idx vars blockVar }

      return args

    computePhi idx block@SBlock{..}
      = block{ sbPhis = map computePhi' sbPhis }
      where
        computePhi' phi@SPhi{..} = fromJust $ do
          x <- Map.lookup spDest (vars' scope)
          prev <- Set.toList <$> Map.lookup idx graph'
          return phi{ spMerge = mapMaybe (lookupVar x) prev }

        lookupVar var block = do
          vars <- Map.lookup block blockVar
          (_, var) <- Map.lookup var vars
          return (block, var)


-- | Generates unoptimised Scratchy code for a program.
generateS :: IProgram -> SProgram
generateS IProgram{..}
  = SProgram (map genFunc ipFuncs ++ coreFunctions)
