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
import           Data.Maybe
import           Whacked.AST
import           Whacked.IMF



data Scope
  = Scope
    { function :: AFunction
    , nextTemp :: ITemp
    , vars :: Map String ITemp
    }
  deriving (Eq, Ord, Show)


newtype Generator a
  = Generator
    { run :: StateT Scope (Writer [IInstr]) a
    }
  deriving (Applicative, Functor, Monad, MonadState Scope, MonadWriter [IInstr])


genTemp :: Generator ITemp
genTemp = do
  scope@Scope{ nextTemp } <- get
  put scope{ nextTemp = nextTemp + 1}
  return nextTemp


getVar :: String -> Generator ITemp
getVar name = do
  scope@Scope{ nextTemp, vars } <- get
  case Map.lookup name vars of
    Just loc -> return loc
    Nothing -> do
      put scope{ nextTemp = nextTemp + 1, vars = Map.insert name nextTemp vars }
      return nextTemp


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
  tell [IBinOp IInt aeBinOp temp leftTemp rightTemp]
  return (IInt, temp)
genExpr AVar{..}  = do
  temp <- getVar aeName
  return (IInt, temp)
genExpr AConstInt{..}  = do
  temp <- genTemp
  tell [IConstInt temp aeIntVal]
  return (IInt, temp)
genExpr ACall{..} = do
  temp <- genTemp
  args <- mapM genExpr aeArgs
  tell [ICall (Just (IInt, temp)) aeName (map snd args)]
  return (IInt, temp)


genStmt :: AStatement -> Generator ()
genStmt AReturn{..} = do
  (t, temp) <- genExpr asExpr
  tell [IReturn t temp]
genStmt APrint{..} = do
  (t, temp) <- genExpr asExpr
  tell [ICall Nothing "__print_int" [temp]]
genStmt AAssign{..} = do
  case asTo of
    ALVar{..} -> do
      -- Increment temp count to create a new version for the variable.
      scope@Scope{ nextTemp } <- get
      (t, temp') <- genExpr asExpr

      -- Update the variable's version.
      -- If the expression was a simple variable reference, then genExpr
      -- returned the ID of that variable. In this case, the variable name
      -- will point to the ID of the source value. Otherwise, the name
      -- of the new variable will be the ID of the result of the expression.
      scope@Scope{ vars } <- get
      put scope{ vars = Map.insert alName temp' vars}


genFunction :: AFunction -> IFunction
genFunction func@AFunction{..}
  = IFunction afName instrs
  where
    instrs
      = execWriter . evalStateT (run gen) $ scope
    scope
      = Scope
        { function = func
        , nextTemp = 0
        , vars = Map.empty
        }
    gen = do
      forM_ afArgs $ \AArg{..} -> do
        temp <- getVar aaName
        tell [IArg IInt temp]
      mapM_ genStmt afBody


generate :: AProgram -> IProgram
generate (AProgram functions)
  = IProgram $ map genFunction functions