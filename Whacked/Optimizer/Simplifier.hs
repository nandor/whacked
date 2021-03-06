{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.Optimizer.Simplifier
  ( moveConstants
  , simplify
  , flatten
  , pruneFlowGraph
  , pruneCallGraph
  ) where

import           Control.Applicative
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set(Set)
import qualified Data.Set as Set
import           Whacked.FlowGraph
import           Whacked.Scratch
import           Whacked.Types



-- |Pushes immediate constants into instructions. Due to the fact that ARM
-- instructions can encode values up to 8 bits in side, the number of required
-- registers will be reduced after doing this.
moveConstants :: SFunction -> SFunction
moveConstants func@SFunction{..}
  = func{ sfBlocks = sfBlocks' }
  where
    (_, sfBlocks')
      = Map.foldlWithKey replaceBlock (Map.empty, Map.empty) sfBlocks

    replaceBlock (vars, blocks) idx block@SBlock{..}
      = (vars', Map.insert idx block{ sbInstrs = reverse sbInstrs' } blocks)
      where
        (vars', sbInstrs') = foldl replaceInstr (vars, []) sbInstrs

    replaceInstr (vars, instrs) instr
      = case instr of
        op@SBinOp{..} ->
          (vars, op{ siRight = replaceVar siRight }:instrs)
        op@SUnOp{..} ->
          (vars, op{ siArg = replaceVar siArg }: instrs)
        op@SMov{..} ->
          (vars, op{ siArg = replaceVar siArg }:instrs)
        op@SBool{..} ->
          (Map.insert siDest (SImm $ fromEnum siBool) vars, op:instrs)
        op@SInt{..} | fitsInImm siInt ->
          (Map.insert siDest (SImm siInt) vars, op:instrs)
        op@SWriteArray{..} ->
          ( vars
          , op
            { siIndex = replaceVar siIndex
            , siExpr = replaceVar siExpr
            }:instrs
          )
        op@SReadArray{..} ->
          (vars, op{ siIndex = replaceVar siIndex }:instrs)
        op@SBinJump{..} ->
          (vars, op{ siRight = replaceVar siRight }:instrs)
        op@SReturn{..} ->
          (vars, op{ siArg = replaceVar siArg }:instrs)
        op@SCheckNull{..} ->
          (vars, op{ siArg = replaceVar siArg }:instrs)
        op@SCall{..} ->
          (vars, op{ siArgs = map replaceVar siArgs }:instrs)
        _ ->
          (vars, instr:instrs)
      where
        replaceVar var = fromMaybe var $ Map.lookup var vars


-- |Replaces some instructions with calls to functions from glibc or custom
-- wrapper functions.
simplify :: SFunction -> SFunction
simplify func@SFunction{..}
  = concatMapI replace func
  where
    replace op@SBinOp{..}
      = case siBinOp of
          Div ->
            [ SCall [siDest] "__aeabi_idiv" [siLeft, siRight] ]
          Mod ->
            [ SCall [SVar (-1), siDest] "__aeabi_idivmod" [siLeft, siRight] ]
          x ->
            [op]
    replace op@SUnOp{..}
      = case siUnOp of
          Len -> [SLength siDest siArg]
          x -> [op]
    replace SNewArray{..}
      = [ SCall [siDest] "__alloc" [ SImm siLength, SImm (sizeof siType) ] ]
    replace SNewPair{..}
      = [ SCall [siDest] "__alloc" [ SImm 2, SImm 4 ] ]
    replace SFree{..}
      = [ SCall [] "__delete" [siDest] ]
    replace x
      = [ x ]


-- | Flattens the program, removing blocks.
flatten :: SFunction -> SFunction
flatten func@SFunction{..}
  = SFlatFunction (map relabel instrs') sfArgs sfName
  where
    (mp, _, instrs')
      = Map.foldlWithKey transform (Map.empty, 0, []) sfBlocks

    transform (mp, next, acc) idx SBlock{..}
      = ( Map.insert idx next mp
        , next + length sbInstrs
        , acc ++ zip [next..] sbInstrs
        )

    relabel (i, op@SJump{..})
      = (i, op{ siWhere = fromJust $ Map.lookup siWhere mp })
    relabel (i, op@SBinJump{..})
      = (i, op{ siWhere = fromJust $ Map.lookup siWhere mp })
    relabel (i, op@SUnJump{..})
      = (i, op{ siWhere = fromJust $ Map.lookup siWhere mp })
    relabel (i, x)
      = (i, x)


-- |Removes unreachable instructions.
pruneFlowGraph :: SFunction -> SFunction
pruneFlowGraph func@SFlatFunction{..}
  = func{ sfInstrs = prune sfInstrs }
  where
    nextNode = Map.fromList . zip (map fst sfInstrs) . tail . map fst $ sfInstrs
    next i = fromMaybe [] $ (:[]) <$> Map.lookup i nextNode

    cfg = foldr (\(i, x) -> Map.insert i (getNext i x)) Map.empty sfInstrs
    getNext i SUnJump{..}
      = siWhere : next i
    getNext i SBinJump{..}
      = siWhere : next i
    getNext i SJump{..}
      = [siWhere]
    getNext i SReturn{..}
      = []
    getNext i SCall{ siFunc = "exit" }
      = []
    getNext i _
      = next i

    cfg' = Map.foldlWithKey (\mp x next -> reverse x next mp) Map.empty cfg
    reverse node next mp
      = foldr (\x -> Map.insertWith (++) x [node]) mp next

    reachable = dfs cfg [0] Set.empty

    prune ((i, x):xs)
      | not (i `Set.member` reachable) = prune xs
      | otherwise = (i, x) : prune xs
      where
        canRemove i' siWhere
          = i < siWhere && i' >= siWhere
    prune []
      = []

-- |Removes functions that are not called.
pruneCallGraph :: SProgram -> SProgram
pruneCallGraph prog@SProgram{..}
  = prog{ spFuncs = [ x | x <- spFuncs, sfName x `Set.member` called] }
  where
    graph
      = foldr (\f -> Map.insertWith (++) (sfName f) (calls f)) Map.empty spFuncs

    calls SFlatFunction{..}
      = concatMap (getCalls . snd) sfInstrs
    calls _
      = []

    getCalls SCall{..}
      = [siFunc]
    getCalls SCheckNull{..}
      = ["__check_null"]
    getCalls SCheckZero{..}
      = ["__check_zero"]
    getCalls SCheckBounds{..}
      = ["__check_bounds"]
    getCalls SBinOp{..}
      | siBinOp `elem` [ Add, Sub, Mul ] = [ "__check_overflow" ]
      | otherwise = []
    getCalls SUnOp{ siUnOp = Neg }
      = [ "__check_overflow" ]
    getCalls _
      = []

    called = dfs graph ["main"] Set.empty


-- |Depth first traversal over a graph.
dfs :: (Ord a, Eq a) => Map a [a] -> [a] -> Set a -> Set a
dfs graph
  = dfs'
  where
    dfs' (x:xs) set
      | x `Set.member` set
        = dfs' xs set
      | otherwise
        = dfs' (fromMaybe [] (Map.lookup x graph) ++ xs) (Set.insert x set)
    dfs' [] set
      = set