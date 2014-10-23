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
import           Whacked.FlowGraph
import           Whacked.Itch
import           Whacked.Tree
import           Whacked.Types


import Debug.Trace
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
    Left err -> throwError err
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
genExpr AUnOp{..} = do
  (t, expr) <- genExpr aeArg
  case unOpType aeUnOp t of
    Nothing ->
      genError aeTag "type error: mismatched types"
    Just t ->
      return (t, IUnOp t aeUnOp expr)
genExpr ABinOp{..} = do
  (lt, le) <- genExpr aeLeft
  (rt, re) <- genExpr aeRight
  case binOpType aeBinOp lt rt of
    Nothing ->
      genError aeTag "type error: mismatched types"
    Just t ->
      return (t, IBinOp t aeBinOp le re)
genExpr AVar{..} = findVar aeName >>= \case
  Nothing ->
    genError aeTag $ "undefined variable '" ++ aeName ++ "'"
  Just (idx, t) ->
    return (t, IVar t aeName idx)
genExpr AInt{..} =
  return (Int, IInt aeInt)
genExpr ABool{..} =
  return (Bool, IBool aeBool)
genExpr AChar{..} =
  return (Char, IChar aeChar)
genExpr AString{..} =
  return (Array Char, IArray (Array Char) (map IChar aeString))
genExpr AIndex{..} = do
  (at, aexpr) <- genExpr aeArray
  (it, iexpr) <- genExpr aeIndex
  when (it /= Int) $
    genError aeTag $ "type error: integer expected"
  case at of
    Array at' ->
      return (at', IIndex at' aexpr iexpr)
    _ ->
      genError aeTag $ "type error: array expected"
genExpr ANull{..} =
  return (Null, INull)


-- |Generates code to convert a rvalue to an expression.
genRValue :: ARValue -> Generator (Type, IExpr)
genRValue ARExpr{..} =
  genExpr arExpr
genRValue ARArray{..} =
  mapM genExpr arElems >>= \case
    [] ->
      return (Empty, IArray Empty [])
    ((t, x):xs) | all (\(y, _) -> y == t) xs ->
      return (Array t, IArray t $ x:(map snd xs))
    _ ->
      genError arTag $ "type error: invalid element"
genRValue ARPair{..} = do
  (le, lexpr) <- genExpr arFst
  (re, rexpr) <- genExpr arSnd
  return (Pair le re, IPair (Pair le re) lexpr rexpr)
genRValue ARElem{..} = do
  (pe, pexpr) <- genExpr arPair
  case pe of
    Pair lp rp | arElem == Fst ->
      return (lp, IElem lp pexpr Fst)
    Pair lp rp | arElem == Snd ->
      return (rp, IElem rp pexpr Snd)
    _ ->
      genError arTag "type error: pair expected"
genRValue ARCall{..} = do
  Scope{ functions } <- get
  case Map.lookup arName functions of
    Nothing ->
      genError arTag $ "undefined function '" ++ arName ++ "'"
    Just AFunction{ afType, afArgs } -> do
      when (length afArgs /= length arArgs) $
        genError arTag $ "invalid number of arguments"
      args <- forM (zip afArgs arArgs) $ \(AArg{ aaType }, arg) -> do
        (t, expr) <- genExpr arg
        when (not (t `match` aaType)) $
          genError arTag $ "type error: invalid argument"
        return expr
      return (afType, ICall afType arName args)


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


-- |Generates code for an asignment instruction.
genAssignment :: ALValue -> Type -> IExpr -> Generator ()
genAssignment ALArray{..} et eexpr = do
  (at, aexpr) <- genExpr alArray
  (it, iexpr) <- genExpr alIndex

  when (it /= Int) $
    genError alTag "type error: integer expected"
  case at of
    Array t -> do
      when (t /= et) $
        genError alTag "type error: mismatched types"

    _ -> do
      genError alTag "type error: array expected"


-- |Generates code for a statement.
genStmt :: AStatement -> Generator ()
genStmt ASkip{..} = do
  -- Ignore skip statements
  return ()

genStmt AVarDecl{..} = do
  -- Insert variables in the scope. Checks if the variable has not been already
  -- declared in the same scope, generates an assignment instruction & places
  -- the variable in the symbol table.
  scope@Scope{ nextScope, variables = (_, x):xs, declarations } <- get

  -- There must be no other declaration in the same scope.
  var <- findVar asName
  when (isJust var) $ do
    let Just (idx, t) = var
    when (idx == nextScope) $
      genError asTag $ "duplicate variable '" ++ asName ++ "'"

  -- Check the types
  (et, eexpr) <- genRValue asWhat
  when (not $ asType `match` et) $
    genError asTag $ "type error: mismatched types"

  -- Generate an assignment instruction.
  tell [IAssVar asName nextScope eexpr]

  -- Update the symbol table.
  put scope
    { variables = (nextScope, Map.insert asName asType x) : xs
    , declarations = Set.insert (asName, nextScope, asType) declarations
    }

genStmt AAssign{..} = do
  -- Generates code for assignments. Checks if the variable has been defined
  -- beforehand & checks whether the types are correct.
  (et, eexpr) <- genRValue asWhat
  genAssignment asTo et eexpr


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

genStmt ARead{..}
  = findVar (alName asTo) >>= \case
      Nothing -> genError asTag $ "undefined variable '" ++ (alName asTo) ++ "'"
      Just (idx, t) -> case asTo of
        ALVar{..} | isReadable t->
          tell [IRead alName idx t]
        ALVar{..} ->
          genError alTag "type cannot be read"
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
genStmt AEnd
  = tell [IEnd]


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
generateI (AProgram functions) = do
  funcs <- forM (Map.toList functions') $ \func -> do
    ifunc <- generate' func
    unless (checkFlowGraph ifunc) $
      throwError "not all control paths terminate"
    return ifunc
  return $ IProgram funcs
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
          , ifBody = body
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