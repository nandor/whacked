{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Whacked.FlowGraph
  ( FlowGraph
  , relabel
  , removePhi
  , buildFlowGraph
  , checkFlowGraph
  , allPathsReturn
  ) where


import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Tree
import           Whacked.Itch
import           Whacked.Scratch
import           Whacked.Types



type FlowGraph
  = Map Int [Int]


-- |Checks if all the paths of the AST return. If not all paths return from
-- a function, a syntax error is thrown. The only messy case is represented
-- by if statements: either both paths return or there is a return after
-- the if statement. The statements that are considered terminal are exit
-- and return statements.
allPathsReturn :: [AStatement] -> Bool
allPathsReturn []
  = False
allPathsReturn (AIf{..}:ps)
  = (allPathsReturn asTrue && allPathsReturn asFalse) || allPathsReturn ps
allPathsReturn (ABlock{..}:ps)
  = allPathsReturn asBody || allPathsReturn ps
allPathsReturn (AReturn{}:ps)
  = True
allPathsReturn (AExit{}:ps)
  = True
allPathsReturn (p:ps)
  = allPathsReturn ps


-- | Checks whether all paths terminate.
checkFlowGraph :: IFunction -> Bool
checkFlowGraph IFunction{..}
  = all (\x -> Set.member x terminals) . map fst $ Map.toList body
  where
    body = Map.fromList $ zip [0..] ifBody

    terminals = dfs [x | (x, i) <- Map.toList body, isTerminal i] Set.empty
    dfs [] viz
      = viz
    dfs (x:xs) viz
      | Set.member x viz = dfs xs viz
      | Just xs' <- Map.lookup x prev = dfs (xs' ++ xs) (Set.insert x viz)
      | otherwise = dfs xs (Set.insert x viz)
    prev
      = Map.fromList
      . zip (tail . map fst . Map.toList $ body)
      . map (\x -> [x])
      $ [0..]

    labels = Map.foldlWithKey getLabel Map.empty body
    getLabel mp i ILabel{ iiLabel }
      = Map.insert iiLabel i mp
    getLabel mp _ _
      = mp

    prev' = Map.foldlWithKey getJump prev body
    getJump mp i IBinJump{ iiWhere }
      = Map.insertWith (++) i [fromJust $ Map.lookup iiWhere labels] mp
    getJump mp i IUnJump{ iiWhere }
      = undefined
    getJump mp i IJump{ iiWhere }
      = undefined
    getJump mp _ _
      = mp

    isTerminal IReturn{}
      = True
    isTerminal IExit{}
      = True
    isTerminal IEnd{}
      = True
    isTerminal _
      = False


-- |Relabels the instructions after eliminating nodes.
relabel :: SFunction -> SFunction
relabel func@SFunction{..}
  = func{ sfBody = map relabel' sfBody }
  where
    labels = Map.fromList (genLabels 0 0 (map fst sfBody))
    genLabels n cnt (x:xs)
      = zip [n..] (replicate (x - n + 1) cnt) ++ genLabels (x + 1) (cnt + 1) xs
    genLabels n _ []
      = []

    update x = fromJust $ Map.lookup x labels

    relabel' (i, jmp@SBinJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, jmp@SUnJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, jmp@SJump{..})
      = (update i, jmp{ siWhere = update siWhere })
    relabel' (i, x)
      = (update i, x)


-- | Removes phi nodes from the code.
-- | TODO(nl1813): Fix phi renaming.
removePhi :: SFunction -> SFunction
removePhi func@SFunction{..}
  = func
    { sfBody = [ instr | Just instr <- map removePhi' sfBody ]
    , sfArgs = map replace sfArgs
    }
  where
    (alias, _) = foldr findAlias (foldl newAlias (Map.empty, 0) sfArgs) sfBody
    findAlias (_, SPhi{..}) (alias, next)
      = case Map.lookup siDest alias of
          Nothing ->
            ( foldl (\mp x -> Map.insert x (SVar next) mp)
             (Map.insert siDest (SVar next) alias)
             siMerge
            , next + 1
            )
          Just var ->
            ( foldl (\mp x -> Map.insert x var mp) alias siMerge
            , next
            )
    findAlias (_, x) (alias, next)
      = foldl newAlias (alias, next) (getGen x)

    newAlias (alias, next) var
      = case Map.lookup var alias of
          Nothing -> ( Map.insert var (SVar next) alias, next + 1)
          Just _ -> ( alias, next )

    replace var
      = fromMaybe var $ Map.lookup var alias

    removePhi' (i, SPhi{..})
      = Nothing
    removePhi' (i, call@SCall{..})
      = Just
        (i, call{ siRet = map replace siRet, siArgs = map replace siArgs})
    removePhi' (i, int@SConstInt{..})
      = Just (i, int{ siDest = replace siDest })
    removePhi' (i, chr@SConstChar{..})
      = Just (i, chr{ siDest = replace siDest })
    removePhi' (i, bool@SConstBool{..})
      = Just (i, bool{ siDest = replace siDest })
    removePhi' (i, string@SConstString{..})
      = Just (i, string{ siDest = replace siDest })
    removePhi' (i, write@SWriteArray{..})
      = Just
        ( i
        , write
          { siDest = replace siDest
          , siArg = replace siArg
          , siIndex = replace siIndex
          , siExpr = replace siExpr
          }
        )
    removePhi' (i, jmp@SBinJump{..})
      = Just (i, jmp{ siLeft = replace siLeft, siRight = replace siRight })
    removePhi' (i, jmp@SUnJump{..})
      = Just (i, jmp{ siVal = replace siVal })
    removePhi' (i, jmp@SJump{..})
      = Just (i, jmp)
    removePhi' (i, bin@SBinOp{..})
      = Just (i, bin
          { siDest = replace siDest
          , siLeft = replace siLeft
          , siRight = replace siRight
          })
    removePhi' (i, un@SUnOp{..})
      = Just (i, un
          { siDest = replace siDest
          , siArg = replace siArg
          })
    removePhi' (i, ret@SReturn{..})
      = Just (i, ret{ siVal = replace siVal })


-- | Builds the control flow graph.
buildFlowGraph :: [(Int, SInstr)] -> (FlowGraph, FlowGraph)
buildFlowGraph block
  = (cfg, cfg')
  where
    cfg = Map.fromList . zip (map fst block) . map build $ block
    cfg' = Map.foldlWithKey rev Map.empty cfg
    next
      = Map.fromList $ zip (map fst block) (tail . map (\(x, _) -> [x]) $ block)

    (count, _) = last block

    build (idx, SReturn{..})
      = []
    build (idx, SJump{..})
      = [siWhere]
    build (idx, SBinJump{..})
      | idx < count = siWhere : fromMaybe [] (Map.lookup idx next)
      | otherwise = [siWhere]
    build (idx, SUnJump{..})
      | idx < count = siWhere : fromMaybe [] (Map.lookup idx next)
      | otherwise = [siWhere]
    build (idx, _)
      | idx < count = fromMaybe [] (Map.lookup idx next)
      | otherwise = []

    rev cfg' node out
      = foldl (\cfg' x -> Map.insertWith (++) x [node] cfg') cfg' out
