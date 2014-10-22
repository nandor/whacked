{-# LANGUAGE GeneralizedNewtypeDeriving,
             LambdaCase,
             NamedFieldPuns,
             RecordWildCards #-}
module Whacked.Frontend.Generator
  ( generateI
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Whacked.Itch
import           Whacked.Tree
import           Whacked.Types



-- |The scope keeps information about declared variables, functions return
-- types, label counters and scope counters. It is wrapped into a state monad
-- for easy access.
data Scope
  = Scope
    { function     :: String
    , functions    :: Map String AFunction
    , nextScope    :: Int
    , nextLabel    :: Int
    , variables    :: [(Int, Map String Type)]
    , declarations :: Set (String, Int, Type)
    }
  deriving ( Eq, Ord, Show )


-- |The generator monad obtained by stacking a state, a writer and an exception
-- monad. The state monad keeps information about variable counters and scope,
-- the writer monad emits instructions and the exception monad is use to
-- report semantic errors.
newtype Generator a
  = Generator (StateT Scope (WriterT [IInstr] (Except String)) a)
  deriving ( Applicative
           , Functor
           , Monad
           , MonadError String
           , MonadState Scope
           , MonadWriter [IInstr]
           )


-- |This function is a wrapper which takes a generator and a scope and returns
-- either the error message produced by running the generator or the generated
-- value, the new scope and the emitted instructions.
runGenerator :: Generator a -> Scope -> Either String (a, Scope, [IInstr])
runGenerator (Generator gen) scope = do
  ((a, scope'), instr) <- runExcept . runWriterT . runStateT gen $ scope
  return (a, scope', instr)


-- |This function executes a generator in an isolated scope. It returns the
-- sequence of instructions generated. It also increments the nextScope value.
-- The effect of this function is that the generated code will correctly
-- increment all the counters, but variables declared in the isolated scope
-- will be restricted to that scope.
scope :: Generator a -> Generator (a, [IInstr])
scope gen = do
  -- Create a new variable scope by incrementing the scope counter.
  scope@Scope{ nextScope, variables } <- get
  let scope' = scope
        { nextScope = nextScope + 1
        , variables = (nextScope + 1, Map.empty) : variables
        }
  put scope'

  -- Run the isolated generator & propagate errors and scopes.
  case runGenerator gen scope' of
    Left err -> fail err
    Right (a, scope''@Scope{ variables }, instr) -> do
      put scope''{ variables = tail variables }
      return (a, instr)


-- |Finds a variable in the scope. Returns the variable from the scope
-- that is closes to the current scope.
findVar :: String -> Generator (Maybe (Int, Type))
findVar name
  = get >>= \scope@Scope{ variables } -> return $ findVar' variables
  where
    findVar' []
      = Nothing
    findVar' ((idx, x):xs)
      | Just x' <- Map.lookup name x = Just (idx, x')
      | otherwise = findVar' xs


-- |Returns the index of the next available label. Indices are guaranteed to
-- be unique, but they are not necessarily continous.
getLabel :: Generator Int
getLabel = do
  scope@Scope{ nextLabel } <- get
  put scope{ nextLabel = nextLabel + 1 }
  return nextLabel


-- |Reports a formatted error message, including the tag of the token.
genError :: ATag -> String -> Generator a
genError ATag{..} msg
  = throwError $
      show atSource ++
      " (line " ++ show atLine ++ ", " ++ show atChar ++ "):\n" ++
      msg


-- |Transforms the AST expression to IMF expressions, checking types and
-- removing tags.
genExpr :: AExpr -> Generator (Type, IExpr)
genExpr ABinOp{..} = do
  (lt, le) <- genExpr aeLeft
  (rt, re) <- genExpr aeRight
  case binOpType aeBinOp lt rt of
    Nothing -> genError aeTag "type error"
    Just t -> return (t, IBinOp t aeBinOp le re)
genExpr AUnOp{..} = do
  (t, expr) <- genExpr aeArg
  case unOpType aeUnOp t of
    Nothing -> genError aeTag "type error"
    Just t -> return (t, IUnOp t aeUnOp expr)
genExpr AVar{..} = do
  findVar aeName >>= \case
    Nothing -> genError aeTag $ "undefined variable '" ++ aeName ++ "'"
    Just (idx, t) -> return (t, IVar t aeName idx)
genExpr AConstInt{..} = do
  return (Int, IConstInt aeIntVal)
genExpr AConstBool{..} = do
  return (Bool, IConstBool aeBoolVal)
genExpr AConstChar{..} = do
  return (Char, IConstChar aeCharVal)
genExpr AConstString{..} = do
  return (String, IConstString aeStringVal)
genExpr ACall{..} = do
  get >>= \Scope{ functions } -> case Map.lookup aeName functions of
    Nothing -> genError aeTag $ "undefined function '" ++ aeName ++ "'"
    Just AFunction{ afType, afArgs } -> do
      when (length afArgs /= length aeArgs) $
        genError aeTag $ "invalid number of arguments"
      args <- forM (zip afArgs aeArgs) $ \(AArg{ aaType }, arg) -> do
        (t, expr) <- genExpr arg
        when (not (t `match` aaType)) $
          genError aeTag $ "type error"
        return expr
      return (afType, ICall afType aeName args)
genExpr ANewPair{..} = do
  (lt, le) <- genExpr aeFst
  (rt, re) <- genExpr aeSnd
  return (Pair lt rt, INewPair (Pair lt rt) le re)


-- |Generates a jump statement.
genJump :: Int -> Bool -> IExpr -> Generator ()
genJump target branch expr@IBinOp{..}
  = case ieBinOp of
      And | branch -> do
        end <- getLabel
        genJump end False ieLeft
        genJump target True ieRight
        tell [ ILabel end ]
      And -> do
        genJump target False ieLeft
        genJump target False ieRight
      Or | branch -> do
        genJump target True ieLeft
        genJump target True ieRight
      Or -> do
        end <- getLabel
        genJump end True ieLeft
        genJump target False ieRight
        tell [ ILabel end ]
      Cmp op -> do
        tell [ IBinJump target branch op ieLeft ieRight ]
      op ->
        tell [ IUnJump target branch expr ]
genJump target branch expr
  = case expr of
    IUnOp{ ieUnOp = Not, .. } ->
      tell [IUnJump target (not branch) ieArg ]
    _ ->
      tell [ IUnJump target branch expr]


-- |Generates code for an assignment statement.
genAssignment :: Type -> ATag -> String -> ARValue -> Generator ()
genAssignment declType tag name ARExpr{..} = do
  scope@Scope{ nextScope, variables = (_, x):xs, declarations } <- get
  (t, expr') <- genExpr arExpr
  when (not (t `match` declType)) $
    genError tag $ "type error"
  tell [IWriteVar name nextScope expr']
  put scope
    { variables = (nextScope, Map.insert name declType x) : xs
    , declarations = Set.insert (name, nextScope, declType) declarations
    }
genAssignment declType tag name ARArray{..} = do
  scope@Scope{ nextScope, variables = (_, x):xs, declarations } <- get
  case declType of
    Array t n | n > 0 -> do
      vals <- mapM genExpr arElems
      forM_ vals $ \(t', _) ->
        when (not (t' `match` (elemType (Array t n)))) $
          genError tag $ "type error"
      tell [INewArray name nextScope (map snd vals)]
      put scope
        { variables = (nextScope, Map.insert name declType x) : xs
        , declarations = Set.insert (name, nextScope, declType) declarations
        }
    _ -> genError tag $ "type error"


-- |Generates code for a statement.
genStmt :: AStatement -> Generator ()
genStmt AReturn{..} = do
  scope@Scope{ function, functions } <- get
  case Map.lookup function functions of
    Nothing -> genError asTag "function not found"
    Just AFunction{ afType } -> do
      (t, expr) <- genExpr asExpr
      when (not (t `match` afType)) $ do
        genError asTag "invalid return type"
      tell [IReturn expr]
genStmt APrint{..} = do
  (t, expr) <- genExpr asExpr
  tell [IPrint expr]
genStmt APrintln{..} = do
  (t, expr) <- genExpr asExpr
  tell [IPrintln expr]
genStmt AAssign{..}
  = findVar (alName asTo) >>= \case
      Nothing -> genError asTag $ "undefined variable '" ++ (alName asTo) ++ "'"
      Just (idx, t) -> case asTo of
        ALVar{..} -> do
            (t', expr) <- genExpr asExpr
            when (not (t `match` t')) $
              genError alTag $ "type error"
            tell [IWriteVar alName idx expr]
        ALArray{..} -> do
          (t', expr') <- genExpr alIndex
          (t'', expr'') <- genExpr asExpr
          when (t' /= Int) $
            genError alTag "indices must be integers"
          case t of
            String | t'' == Char ->
              tell [IWriteArray alName idx expr' expr'']
            Array t n | n  == 1 && t == t'' ->
              tell [IWriteArray alName idx expr' expr'']
            _ -> genError alTag $ "'" ++ alName ++ "' is not an array"
genStmt ARead{..}
  = findVar (alName asTo) >>= \case
      Nothing -> genError asTag $ "undefined variable '" ++ (alName asTo) ++ "'"
      Just (idx, t) -> case asTo of
        ALVar{..} | isReadable t->
          tell [IRead alName idx t]
        ALVar{..} ->
          genError alTag "type cannot be read"
genStmt AVarDecl{..} = do
  forM_ asVars $ \(tag, name, expr) -> do
    scope@Scope{ nextScope, variables = (_, x):xs, declarations } <- get
    var <- findVar name
    when (isJust var) $ do
      let Just (idx, t) = var
      when (idx == nextScope) $
        genError asTag $ "duplicate variable '" ++ name ++ "'"
    genAssignment asType tag name expr

genStmt AWhile{..} = do
  (t, expr) <- genExpr asExpr
  when (not (t `match` Bool)) $ do
    genError asTag $ "boolean expected"

  start <- getLabel
  (_, body) <- scope (mapM_ genStmt asBody)
  end <- getLabel

  genJump end False expr
  tell [ILabel start]
  tell body
  genJump start True expr
  tell [ILabel end]
genStmt AIf{..} = do
  (t, expr) <- genExpr asExpr
  when (not (t `match` Bool)) $ do
    genError asTag $ "boolean expected"

  if asFalse == []
    then do
      end <- getLabel
      genJump end False expr
      (_, true') <- scope (mapM_ genStmt asTrue)
      tell [ILabel end]
    else do
      [false, end] <- replicateM 2 getLabel
      (_, true') <- scope (mapM_ genStmt asTrue)
      (_, false') <- scope (mapM_ genStmt asFalse)

      genJump false False expr
      tell true'
      tell [IJump end]
      tell [ILabel false]
      tell false'
      tell [ILabel end]
genStmt AFree{..} = do
  (t, expr) <- genExpr asExpr
  case t of
    Pair _ _ -> tell [IFree expr]
    _ -> genError asTag $ "free can only be used on pairs"
genStmt ABlock{..} = do
  (_, instrs) <- scope $ mapM_ genStmt asBody
  tell instrs
genStmt AExit{..} = do
  (t, expr) <- genExpr asExpr
  when (not (t `match` Int)) $ do
    genError asTag $ "integer expected"
  tell [IExit expr]


-- |Generates code for a function body.
genFunc :: AFunction -> Generator ()
genFunc AFunction{..} = do
  -- |Put all arguments into the scope.
  scope@Scope{ nextScope, variables, functions } <- get
  (args, decls) <- foldM (\(args, decls) AArg{..} ->
    case Map.lookup aaName args of
      Nothing -> return
        ( Map.insert aaName aaType args
        , Set.insert (aaName, nextScope, aaType) decls
        )
      Just _ -> genError aaTag $ "duplicate argument '" ++ aaName ++ "'")
    (Map.empty, Set.empty) afArgs
  put scope
    { variables = (nextScope, args) : variables
    , declarations = decls
    }

  -- |Encode statements.
  mapM_ genStmt afBody


-- |Generates code for all the functions in the program. If type checking fails,
-- an error message is returned.
generateI :: AProgram -> Either String IProgram
generateI (AProgram functions)
  = IProgram <$> mapM generate' (Map.toList functions')
  where
    -- |All functions, used to check types.
    functions'
      = Map.fromListWith (++)
      . map (\func -> (afName func, [func]) )
      $ functions

    generate' (_, [func]) = do
      (_, Scope{ declarations }, body) <- runGenerator (genFunc func) scope
      return $ IFunction
          { ifName = afName func
          , ifType = afType func
          , ifArgs = args
          , ifBody = body ++ [IReturn (IConstInt 0)]
          , ifVars = declarations
          }
      where
        -- |All arguments, returned in the structure.
        args
          = map (\(AArg _ t name) -> (t, name)) (afArgs func)

        -- |Initial scope.
        scope
          = Scope
            { function = afName func
            , functions
              = Map.map (\[x] -> x)
              . Map.filter (\x -> length x == 1)
              $ functions'
            , nextScope = 0
            , nextLabel = 0
            , variables = []
            , declarations = Set.empty
            }
    generate' (name, _) = do
      Left $ "redefinition of '" ++ name ++ "'"