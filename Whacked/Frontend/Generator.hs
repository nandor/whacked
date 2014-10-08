{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Whacked.Frontend.Generator
  (generate
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map (Map)
import qualified Data.Map as Map
import           Whacked.AST
import           Whacked.IMF


import Debug.Trace

data Scope
  = Scope
    { function :: AFunction
    , tempIdx :: ITemp
    , instrIdx :: Int
    , vars :: [Map String ITemp]
    }
  deriving ( Eq, Ord, Show )


lookupChain :: String -> [Map String ITemp] -> Maybe ITemp
lookupChain name (x:xs)
  = let var = Map.lookup name x
    in if var == Nothing then lookupChain name xs else var
lookupChain _ []
  = Nothing


newtype Generator a
  = Generator
    { run :: StateT Scope (Writer [(Int, IInstr)]) a
    }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadState Scope
           , MonadWriter [(Int, IInstr)]
           )


emit :: IInstr -> Generator ()
emit instr = do
  scope@Scope{ instrIdx } <- get
  put scope{ instrIdx = instrIdx + 1}
  tell [(instrIdx, instr)]


genBody :: [AStatement] -> Generator [(Int, IInstr)]
genBody instrs = do
  scope <- get
  let scope' = scope{vars = Map.empty : vars scope}
      ((_, s), i) = runWriter. runStateT (run $ mapM_ genStmt instrs) $ scope'
  put s{ vars = tail (vars s) }
  return i


genTemp :: Generator ITemp
genTemp = do
  scope@Scope{ tempIdx } <- get
  put scope{ tempIdx = tempIdx + 1}
  return tempIdx


getVar :: String -> Generator ITemp
getVar name = do
  scope@Scope{ tempIdx, vars } <- get
  case lookupChain name vars of
    Nothing -> do
      let (var:vars') = vars
      put scope
        { tempIdx = tempIdx + 1
        , vars = Map.insert name tempIdx var:vars'
        }
      return tempIdx
    Just loc -> return loc


genTrueJump :: AExpr -> Int -> Generator()
genTrueJump expr target = do
  -- |TODO: implement this.
  emit (IBinJump target ILT 0 0)


genFalseJump :: AExpr -> Int -> Generator()
genFalseJump expr target = do
  -- |TODO: implement this.
  emit (IBinJump target IGT 0 0)


-- |Generates code
genExpr :: AExpr -> Generator (IType, ITemp)
genExpr AUnOp{..}  = do
  temp <- genTemp
  return (IInt, temp)

genExpr ABinOp{..}  = do
  (leftType, leftTemp) <- genExpr aeLeft
  (rightType, rightTemp) <- genExpr aeRight
  -- TODO(nandor): check types
  temp <- genTemp
  emit $ IBinOp IInt aeBinOp temp leftTemp rightTemp
  return (IInt, temp)

genExpr AVar{..}  = do
  temp <- getVar aeName
  return (IInt, temp)

genExpr AConstInt{..}  = do
  temp <- genTemp
  emit $ IConstInt temp aeIntVal
  return (IInt, temp)

genExpr ACall{..} = do
  temp <- genTemp
  args <- mapM genExpr aeArgs
  emit $ ICall (Just (IInt, temp)) aeName (map snd args)
  return (IInt, temp)


genAssign :: String -> AExpr -> Generator ()
genAssign name expr = do
  -- Increment temp count to create a new version for the variable.
  -- TODO: Check type.
  (t, temp') <- genExpr expr

  -- Update the variable's version.
  -- If the expression was a simple variable reference, then genExpr
  -- returned the ID of that variable. In this case, the variable name
  -- will point to the ID of the source value. Otherwise, the name
  -- of the new variable will be the ID of the result of the expression.
  -- Since variables are declared beforehand the name is guaranteed to be
  -- in the scope chain.
  scope@Scope{ vars } <- get
  let updateChain (x:xs)
        | Map.member name x = Map.insert name temp' x : xs
        | otherwise = x : updateChain xs
      updateChain []
        = []
  put scope{ vars = updateChain vars }


genStmt :: AStatement -> Generator ()
genStmt AReturn{..} = do
  (t, temp) <- genExpr asExpr
  emit $ IReturn t temp

genStmt APrint{..} = do
  (t, temp) <- genExpr asExpr
  emit $ ICall Nothing "__print_int" [temp]

genStmt AAssign{..} = do
  case asTo of
    ALVar{..} -> genAssign alName asExpr

genStmt AVarDecl{..} = do
  forM_ asVars $ \(tag, name, expr) -> do
  scope@Scope{ tempIdx, vars = var:vars' } <- get
  put scope{ tempIdx = tempIdx + 1, vars = Map.insert name tempIdx var : vars'}
  case expr of
    Nothing -> return ()
    Just expr' -> genAssign name expr'

genStmt AWhile{..} = do
  start <- (+1) . instrIdx <$> get
  body <- genBody asBody
  end <- (+1) . instrIdx <$> get
  genFalseJump asExpr end
  tell body
  genTrueJump asExpr start

genStmt ABlock{..} = do
  body <- genBody asBody
  tell body

genFunction :: AFunction -> IFunction
genFunction func@AFunction{..}
  = IFunction afName instrs
  where
    instrs
      = execWriter . evalStateT (run gen) $ scope
    scope
      = Scope
        { function = func
        , instrIdx = 0
        , tempIdx = 0
        , vars = [Map.empty]
        }
    gen = do
      forM_ afArgs $ \AArg{..} -> do
        temp <- getVar aaName
        emit $ IArg IInt temp
      mapM_ genStmt afBody


generate :: AProgram -> IProgram
generate (AProgram functions)
  = IProgram $ map genFunction functions