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


genVar :: String -> Generator ITemp
genVar name = do
  scope@Scope{ nextTemp, vars } <- get
  put scope{ nextTemp = nextTemp + 1, vars = Map.insert name nextTemp vars }
  return nextTemp


getVar :: String -> Generator ITemp
getVar name = do
  scope@Scope{ vars } <- get
  return . fromJust $ Map.lookup name vars


genExpr :: AExpr -> Generator (IType, ITemp)
genExpr AUnOp{..} = do
  return (IInt, 0)
genExpr ABinOp{..} = do
  (leftType, leftTemp) <- genExpr aeLeft
  (rightType, rightTemp) <- genExpr aeRight
  temp <- genTemp
  tell [IBinOp IInt aeBinOp temp leftTemp rightTemp]
  return (IInt, temp)
genExpr AVar{..} = do
  temp <- getVar aeName
  return (IInt, temp)
genExpr AConstInt{..} = do
  temp <- genTemp
  tell [IConstInt temp aeIntVal]
  return (IInt, temp)
genExpr ACall{..} = do
  args <- mapM genExpr aeArgs
  temp <- genTemp
  tell [ICall (Just (IInt, temp)) aeName (map snd args)]
  return (IInt, temp)


genStmt :: AStatement -> Generator ()
genStmt AReturn{..} = do
  (t, temp) <- genExpr asExpr
  tell [IReturn t temp]
genStmt APrint{..} = do
  (t, temp) <- genExpr asExpr
  tell [ICall Nothing "__print_int" [temp]]


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
        temp <- genVar aaName
        tell [IArg IInt temp]
      mapM_ genStmt afBody


generate :: AProgram -> IProgram
generate (AProgram functions)
  = IProgram $ map genFunction functions