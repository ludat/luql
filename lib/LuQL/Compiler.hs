{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LuQL.Compiler where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State.Strict (StateT (..), get, modify', put)
import Data.Function (on, (&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (iii)
import Data.Text (Text)
import Data.Void (Void)
import Database.PostgreSQL.Simple.Types (Identifier (..), QualifiedIdentifier (..))
import LuQL.Parser
import LuQL.Types
import Safe (headMay)

data Compiled

type instance StmtE _ "ctx" Compiled = ()

type instance StmtE "from" "model" Compiled = (Text, ModelDefinition)

type instance StmtE "join" "model" Compiled = (Text, ModelDefinition)

type instance StmtE "join" "on" Compiled = (QueryExpression Compiled)

type instance StmtE "ext" "ext" Compiled = CompiledStmtExt

data CompiledStmtExt
  = StmtCompilationFailed (QueryStatement Raw)
  deriving (Show, Eq)

type instance ExprE "prop" "ctx" Compiled = Void

type instance ExprE "lit" "ctx" Compiled = RuntimeType

type instance ExprE "ref" "ctx" Compiled = RuntimeType

type instance ExprE "apply" "ctx" Compiled = RuntimeType

type instance ExprE "apply" "function" Compiled = (Text, [RuntimeType])

type instance ExprE "raw" "ctx" Compiled = RuntimeType

type instance ExprE "if" "ctx" Compiled = RuntimeType

type instance ExprE "computed" "ctx" Compiled = RuntimeType

type instance ExprE "ext" "ext" Compiled = CompiledExprExt

data CompiledExprExt
  = ExprCompilationFailed (QueryExpression Raw)
  | ComputedColumn (ExprE "computed" "ctx" Compiled) ColumnDefinition
  | ComputedModel (ExprE "computed" "ctx" Compiled) Text Identifier (Map TableName (ColumnName, ColumnName))
  | ComputedModelDefinition (ExprE "computed" "ctx" Compiled) ModelDefinition
  | ComputedFunction (ExprE "computed" "ctx" Compiled) FunctionTypeChecker

deriving instance
  ( Show (ExprE "computed" "ctx" Compiled)
  ) =>
  Show CompiledExprExt

deriving instance
  ( Eq (ExprE "computed" "ctx" Compiled)
  ) =>
  Eq CompiledExprExt

instance Show ModelDefinition where
  show ModelDefinition {..} =
    [iii| ModelDefinition
        { tableName = #{show tableName}
        , defaultSingularName = #{show defaultSingularName}
        , columns = #{show columns}
        , relatedTables = #{show relatedTables}
        , implicitWhere = #{maybe "Nothing" (show . ($ "self")) implicitWhere}
        }|]

instance Eq ModelDefinition where
  m1 == m2 =
    m1.tableName == m2.tableName
      && m1.defaultSingularName == m2.defaultSingularName
      && m1.columns == m2.columns
      && m1.relatedTables == m2.relatedTables
      && (((==) `on` (fmap ($ "self") . (.implicitWhere))) m1 m2)

data ModelDefinition = ModelDefinition
  { tableName :: TableName,
    defaultSingularName :: Text,
    columns :: Map ColumnName RuntimeType,
    implicitWhere :: Maybe (Text -> QueryExpression Raw),
    relatedTables :: Map TableName (ColumnName, ColumnName)
  }


instantiateModel :: ModelDefinition -> RuntimeType
instantiateModel model =
  fst $ instantiateModel' model Nothing

instantiateModel' :: ModelDefinition -> Maybe Text -> (RuntimeType, Text)
instantiateModel' model (maybeModelName) =
  let type_ = (ModelType model.columns)
      modelName = fromMaybe model.defaultSingularName maybeModelName
   in (type_, modelName)

data Error
  = CompilerError Text (Int, Int)
  deriving (Eq, Show)

data CompiledQuery = CompiledQuery {unCompiledQuery :: [QueryStatement Compiled]}
  deriving (Show, Eq)

type Models = Map Text ModelDefinition

compileProgram :: Models -> RawQuery -> Either [Error] CompiledQuery
compileProgram models (RawQuery qss) =
  let (compiledStatements, typeInfo) = compileStatements models qss
   in if null typeInfo.errors
        then Right $ CompiledQuery compiledStatements
        else Left typeInfo.errors

compileStatements :: Models -> [QueryStatement Raw] -> ([QueryStatement Compiled], TypeInfo)
compileStatements models qss =
  ( do
      coso <- forM qss $ \qs -> do
        coso <- runExceptT $ compileStatement qs
        case coso of
          Left (qs', ti) -> do
            put ti
            pure [qs']
          Right qs' ->
            pure qs'
      pure $ mconcat coso
  )
    & ( `runStateT`
          ( TypeInfo
              { variablesInScope =
                  Map.toList $
                    fmap
                      ( \m ->
                          ExprExt $ ComputedModelDefinition (ModelDefinitionType) m
                      )
                      models,
                errors = []
              }
          )
      )
    & runIdentity

type CompilerM = ExceptT (QueryStatement Compiled, TypeInfo) (StateT TypeInfo Identity)

compileStatement :: QueryStatement Raw -> ExceptT (QueryStatement Compiled, TypeInfo) (StateT TypeInfo Identity) [QueryStatement Compiled]
compileStatement (From originalPosition (maybeModelAs, modelExpr)) = do
  tcModel <- compileExpression modelExpr
  case tcModel of
    (ExprExt (ComputedModelDefinition _ model)) -> do
      let (runtimeModel, modelName) = instantiateModel' model maybeModelAs
      modify' $ \typeInfo ->
        typeInfo
          { variablesInScope =
              typeInfo.variablesInScope
                ++ [(modelName, ExprExt $ ComputedModel runtimeModel modelName (model.tableName) model.relatedTables)]
          }
      implicitWheresTC <- case model.implicitWhere of
        Just implicitWhere -> compileStatement $ Where (0, 0) $ implicitWhere $ modelName
        Nothing -> pure []
      pure $ [From () (modelName, model)] ++ implicitWheresTC
    -- (ValueMetadata (ModelDefinitionType) (ModelDefinitionSource model)) -> do
    _ -> do
      addError originalPosition [iii|Invalid model|]
      pure $ [] -- [From originalTypeInfo tcModel]
compileStatement (Where originalPosition expression) = do
  newWhere <- compileExpression expression
  unless (getType newWhere `matchesType` BooleanType) $ do
    addError originalPosition [iii|wrong type, expected boolean|]

  pure [Where () newWhere]
compileStatement e@(Join originalPosition (maybeModelAs, modelExpr) mayJoinExpr) = do
  originalTypeInfo <- get

  otherModelExpr <- compileExpression modelExpr
  newModelToJoin <- case otherModelExpr of
    (ExprExt (ComputedModelDefinition _ modelDefinition)) ->
      pure modelDefinition
    _ ->
      addCatastrophicError
        [iii|value to join is not a model|]
        originalPosition
        (StmtExt $ StmtCompilationFailed e)

  let (otherRuntimeModel, otherModelName) = instantiateModel' newModelToJoin maybeModelAs

  modify' $ \typeInfo ->
    typeInfo
      { variablesInScope =
          (otherModelName, ExprExt $ ComputedModel otherRuntimeModel otherModelName newModelToJoin.tableName newModelToJoin.relatedTables)
            : typeInfo.variablesInScope
      }

  joinExpression <-
    case mayJoinExpr of
      Just joinExpr -> compileExpression joinExpr
      Nothing -> do
        (modelToJoin, (myColumn, otherColumn)) <-
          originalTypeInfo.variablesInScope
            & mapMaybe
              ( \case
                  (_, ExprExt (ComputedModel (ModelType cols) modelName tableName _)) -> do
                    (selfCol, foreignCol) <- newModelToJoin.relatedTables Map.!? tableName
                    guard (Map.member foreignCol cols)
                    pure (modelName, (selfCol, foreignCol))
                  _ -> Nothing
              )
            & headMay
            & maybe
              ( do
                  addCatastrophicError
                    [iii|no model to join available|]
                    originalPosition
                    (StmtExt $ StmtCompilationFailed $ e)
              ) pure


        case newModelToJoin.implicitWhere of
          Just where_ ->
            compileExpression $
              Apply
                originalPosition
                (Ref originalPosition "&&")
                [ Apply
                    originalPosition
                    (Ref originalPosition "==")
                    [ Prop originalPosition (Ref originalPosition modelToJoin) $ fromIdentifier otherColumn,
                      Prop originalPosition (Ref originalPosition otherModelName) $ fromIdentifier myColumn
                    ],
                  where_ otherModelName
                ]
          Nothing ->
            compileExpression $
              Apply
                originalPosition
                (Ref originalPosition "==")
                [ Prop originalPosition (Ref originalPosition modelToJoin) $ fromIdentifier otherColumn,
                  Prop originalPosition (Ref originalPosition otherModelName) $ fromIdentifier myColumn
                ]

  pure
    $ [Join () (otherModelName, newModelToJoin) joinExpression]
compileStatement (GroupBy _ groupByColumns letStatements) = do
  _ <- compileAndSetTheNewTypeInfo Tag groupByColumns
  definitions <- mconcat <$> mapM compileStatementAndTag letStatements
  tg <- compileAndSetTheNewTypeInfo Discard groupByColumns
  pure [GroupBy () tg definitions]
  where
    compileStatementAndTag stmt = do
      case stmt of
        Let _ name _ -> do
          ret <- compileStatement stmt
          modifyTypeInfo $ \ty ->
            ty {
              variablesInScope = (
                ty.variablesInScope
                & fmap (\(n, v) -> if n /= name
                  then (n, v)
                  else case v of
                    ExprExt (ComputedColumn t columnDefinition) ->
                      (n, ExprExt $ ComputedColumn (KeptByGroupBy t) columnDefinition)
                    _ ->
                      (n, v)
                  )
              )
            }
          pure ret
        _ ->
          compileStatement stmt
    compileAndSetTheNewTypeInfo tagOrDiscard columns = do
      oldTypeInfo <- get
      compiledColumns <- forM columns $ \(column, name) -> do
        compiledExpression <- compileExpression column
        pure (compiledExpression, name)

      vars <- foldM (\acc (name, expression) -> do
        case expression of
          ExprExt (ComputedModel (ModelType propsMap) modelName tableName relatedTables) -> do
            let relevantColumns =
                  compiledColumns
                  & mapMaybe (\case
                    (ExprExt (ComputedColumn _ (ColumnDefinition (QualifiedIdentifier (Just t) n))), _) | t == modelName -> Just n
                    _ -> Nothing
                    )
                  & fmap Identifier
            pure $ acc ++
              [ (name, ExprExt (ComputedModel
                  (ModelType (propsMap & Map.mapMaybeWithKey (\k v ->
                      case (tagOrDiscard, v) of
                        (Tag, _) -> if k `elem` relevantColumns
                          then Just $ KeptByGroupBy v
                          else Just $ v
                        (Discard, KeptByGroupBy v') -> Just $ v'
                        (Discard, _) -> Nothing)))
                  modelName
                  tableName
                  (relatedTables & Map.filter (\(_, column) -> column `elem` relevantColumns)))
                )
              ]
          ExprExt (ComputedModelDefinition _ _) ->
            pure $ acc ++ [(name, expression)]
          ExprExt (ComputedColumn t columnDefinition) ->
            let relevantColumns =
                  compiledColumns
                  & mapMaybe (\case
                    (ExprExt (ComputedColumn _ (ColumnDefinition (QualifiedIdentifier Nothing n))), _) -> Just n
                    _ -> Nothing
                    )
            in case (tagOrDiscard, t) of
              (Tag, _) ->
                if name `elem` relevantColumns
                  then pure $ acc ++ [(name, ExprExt $ ComputedColumn (KeptByGroupBy t) columnDefinition)]
                  else pure $ acc ++ [(name, ExprExt $ ComputedColumn t columnDefinition)]
              (Discard, KeptByGroupBy t') ->
                  pure $ acc ++ [(name, ExprExt $ ComputedColumn t' columnDefinition)]
              (Discard, _) ->
                pure acc

          _ -> pure acc
        )
        ([] :: [(Text, QueryExpression Compiled)])
        oldTypeInfo.variablesInScope

      modifyTypeInfo $ \ty ->
        ty {
          variablesInScope = vars
        }

      pure compiledColumns

compileStatement (Let _originalPosition name expression) = do
  e <- compileExpression expression

  modifyTypeInfo $ \ti ->
    ti
      { variablesInScope =
          ti.variablesInScope
            ++ [ ( name,
                   ExprExt $ ComputedColumn (getType e) (ColumnDefinition $ QualifiedIdentifier Nothing name)
                 )
               ]
      }

  pure [Let () name e]
compileStatement (Return _ exprs) = do
  tExprs <- mapM compileExpression exprs
  pure [Return () tExprs]
compileStatement (OrderBy _ exprs) = do
  tExprs <- forM exprs $ \(expr, dir) -> do
    compiledExpr <- compileExpression expr
    pure (compiledExpr, dir)
  pure [OrderBy () tExprs]

data TagOrDiscard = Tag | Discard
  deriving (Show, Eq)

matchesType :: RuntimeType -> RuntimeType -> Bool
matchesType v r =
  case (v, r) of
    (AnyType, _) -> True
    (_, AnyType) -> True
    (UnknownType, _) -> True
    (_, UnknownType) -> True
    (KeptByGroupBy a, KeptByGroupBy b) -> a == b
    (KeptByGroupBy a, b) -> matchesType a b
    (_, KeptByGroupBy _) -> False
    (a, b) -> a == b

class (Monad m) => HasTypeInfo m where
  getTypeInfo :: m TypeInfo
  putTypeInfo :: TypeInfo -> m ()

data ColumnDefinition = ColumnDefinition
  { column :: QualifiedIdentifier
  }
  deriving (Show, Eq)

data RuntimeFunction = RuntimeFunction
  { rFuncNotation :: FunctionNotation,
    rFuncName :: Text,
    rFuncCheckTypes :: forall m. (HasTypeInfo m) => (Int, Int) -> [QueryExpression Raw] -> m ([QueryExpression Compiled], RuntimeType)
  }

instance Show RuntimeFunction where
  show RuntimeFunction {..} = [iii|Function #{rFuncName}|]

instance Eq RuntimeFunction where
  f1 == f2 = f1.rFuncName == f2.rFuncName && f1.rFuncNotation == f2.rFuncNotation

data RuntimeType
  = UnknownType
  | AnyType
  | IntType
  | TimestampType
  | FloatType
  | StringType
  | BooleanType
  | DateType
  | NullType
  | ModelType (Map Identifier RuntimeType)
  | ModelDefinitionType
  | FunctionType FunctionTypeChecker
  | KeptByGroupBy RuntimeType
  deriving (Eq, Show)

newtype FunctionTypeChecker
  = FunctionTypeChecker
      ( forall m.
        (HasTypeInfo m) =>
        (Int, Int) ->
        [QueryExpression Raw] ->
        m ([QueryExpression Compiled], RuntimeType, (Text, [RuntimeType]))
      )

instance Show FunctionTypeChecker where
  show _ = "function"

instance Eq FunctionTypeChecker where
  _ == _ = False

data TypeInfo = TypeInfo
  { variablesInScope :: [(Text, QueryExpression Compiled)],
    errors :: [Error]
  }
  deriving (Show)

type TypeCheckM =
  StateT TypeInfo Identity

instance HasTypeInfo CompilerM where
  getTypeInfo =
    get
  putTypeInfo typeInfo =
    put typeInfo

instance (Monad m) => HasTypeInfo (StateT TypeInfo m) where
  getTypeInfo =
    get
  putTypeInfo typeInfo =
    put typeInfo

modifyTypeInfo :: (HasTypeInfo m) => (TypeInfo -> TypeInfo) -> m ()
modifyTypeInfo f = do
  q <- getTypeInfo
  putTypeInfo $ f q

lit :: RuntimeType -> RuntimeType
lit t = t

malo :: RuntimeType
malo = UnknownType

-- TODO esto esta repetido en SqlGeneration.hs
qualifyIdentifier :: Identifier -> Identifier -> QualifiedIdentifier
qualifyIdentifier table column =
  QualifiedIdentifier (Just $ fromIdentifier table) (fromIdentifier column)

compileExpression :: (HasTypeInfo m) => QueryExpression Raw -> m (QueryExpression Compiled)
compileExpression (Lit _ (LiteralString t)) = pure $ Lit (lit StringType) $ LiteralString t
compileExpression (Lit _ (LiteralInt n)) = pure $ Lit (lit IntType) $ LiteralInt n
compileExpression (Lit _ (LiteralFloat n)) = pure $ Lit (lit FloatType) $ LiteralFloat n
compileExpression (Lit _ (LiteralBoolean b)) = pure $ Lit (lit BooleanType) $ LiteralBoolean b
compileExpression (Lit _ (LiteralNull)) = pure $ Lit (lit NullType) LiteralNull
compileExpression expression@(Prop pos expr propName) = do
  tExpr <- compileExpression expr
  case tExpr of
    (ExprExt (ComputedModel (ModelType m) n _ _)) -> case Map.lookup (Identifier propName) m of
      Nothing -> do
        addError pos [iii|#{expr} no tiene una propiedad: #{propName}|]
        pure $ ExprExt $ ExprCompilationFailed $ expression
      Just ty -> do
        pure $
          ExprExt $
            ComputedColumn ty (ColumnDefinition $ qualifyIdentifier (Identifier n) $ Identifier propName)
    -- -- TODO podria definirle propiedades a cosas que no sean tablas :thinking:
    _ -> case getType tExpr of
      IntType -> do
        case propName of
          "even" -> do
            compileExpression
              ( Apply
                  (0, 0)
                  (Ref (0, 0) "==")
                  [ Apply (0, 0) (Ref (0, 0) "%") [expr, Lit (0, 0) $ LiteralInt 2],
                    Lit (0, 0) $ LiteralInt 0
                  ]
              )
          "in" -> do
            let functionTypeChecker = FunctionTypeChecker $ \_ paramExprs -> do
                  tExprs <- forM paramExprs $ \e -> do
                    (getPos e,) <$> compileExpression e
                  case tExprs of
                    [(_p1, e1)] -> do
                      pure ([tExpr, e1], BooleanType, ("in", [getType tExpr, getType e1]))
                    _ -> do
                      addError pos [iii|max expects one parameter|]
                      pure ([], IntType, undefined)
            pure $ ExprExt $ ComputedFunction (FunctionType functionTypeChecker) functionTypeChecker
          _ -> do
            addError pos [iii|Int doesn't have prop '#{propName}'|]
            pure $ ExprExt $ ExprCompilationFailed expression
      StringType -> do
        case propName of
          "length" -> do
            compileExpression
              ( Apply
                  (0, 0)
                  (Ref (0, 0) "char_length")
                  [ expr
                  ]
              )
          _ -> do
            addError pos [iii|Int doesn't have prop '#{propName}'|]
            pure $ ExprExt $ ExprCompilationFailed expression
      DateType -> do
        case propName of
          "between" -> do
            let functionTypeChecker = FunctionTypeChecker $ \_ paramExprs -> do
                  tExprs <- forM paramExprs $ \e -> do
                    (getPos e,) <$> compileExpression e
                  case tExprs of
                    [(p1, e1), (p2, e2)] -> do
                      unless (getType e1 `matchesType` StringType) $
                        addError p1 [iii|expected type is String but got #{getType e1}|]
                      unless (getType e2 `matchesType` StringType) $
                        addError p2 [iii|expected type is String but got #{getType e2}|]
                      pure ([tExpr, e1, e2], BooleanType, ("date_between", [getType tExpr, getType e1, getType e2]))
                    _ -> do
                      addError pos [iii|max expects one parameter|]
                      pure ([], IntType, undefined)
            pure $ ExprExt $ ComputedFunction (FunctionType functionTypeChecker) functionTypeChecker
          "year" -> do
            compileExpression
              ( Apply
                  (0, 0)
                  (Ref (0, 0) "extract_year")
                  [ expr
                  ]
              )
          "month" -> do
            compileExpression
              ( Apply
                  (0, 0)
                  (Ref (0, 0) "extract_month")
                  [ expr
                  ]
              )
          _ -> do
            addError pos [iii|Int doesn't have prop '#{propName}'|]
            pure $ ExprExt $ ExprCompilationFailed expression
      _ -> do
        addError pos [iii|1 eso no tiene una propiedad: #{getType tExpr}|]
        pure $
          ExprExt $
            ExprCompilationFailed expression
compileExpression (Ref originalPosition name) = do
  if name == "_"
    then do
      addError originalPosition [iii|encontre un agujero|]
      pure $ Ref malo name
    else do
      resolveName (addError originalPosition) name
compileExpression (Apply pos function params) = do
  typecheckedFunction <- compileExpression function
  (typecheckedParams, returnType, resolvedFunction) <- case typecheckedFunction of
    (ExprExt (ComputedFunction _ (FunctionTypeChecker checkTypes))) ->
      checkTypes pos params
    value -> do
      addError (getPos function) [iii|No se pueden aplicar #{value}|]
      tExprs <- mapM compileExpression params
      pure (tExprs, malo, ("unknown", []))

  pure $ Apply returnType resolvedFunction typecheckedParams
compileExpression (RawSql _ exprOrText) = do
  coso <- mapM (mapM compileExpression) exprOrText
  pure $ RawSql AnyType coso
compileExpression (If _pos condExpr thenExpr elseExpr) = do
  tcCondExpr <- compileExpression condExpr
  tcThenExpr <- compileExpression thenExpr
  tcElseExpr <- compileExpression elseExpr

  unless (getType tcCondExpr `matchesType` BooleanType) $
    addError (getPos condExpr) [iii|La condicion de un if deberia ser un booleano pero es #{getType tcCondExpr}|]

  unless (getType tcThenExpr `matchesType` getType tcElseExpr) $
    addError (getPos elseExpr) [iii|El else y el then deberian matchear pero #{getType tcThenExpr} es diferente que #{getType tcElseExpr}|]

  pure $ If (getType tcThenExpr) tcCondExpr tcThenExpr tcElseExpr

getType :: QueryExpression Compiled -> RuntimeType
getType (Lit ty _) = ty
getType (Ref ty _) = ty
getType (Apply ty _ _) = ty
getType (RawSql ty _) = ty
getType (If ty _ _ _) = ty
getType (ExprExt (ExprCompilationFailed _)) = UnknownType
getType (ExprExt (ComputedColumn ty _)) = ty
getType (ExprExt (ComputedFunction ty _)) = ty
getType (ExprExt (ComputedModel ty _ _ _)) = ty
getType (ExprExt (ComputedModelDefinition ty _)) = ty

equalityFunction :: Text -> FunctionTypeChecker
equalityFunction name = FunctionTypeChecker $
  \pos paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getPos e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2 || getType e2 == NullType) $
          addError pos [iii|expected types to be equals but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            BooleanType,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError pos [iii|sum expects one argument|]
        pure (fmap snd tExprs, malo, (name, []))

binaryOperator :: Text -> FunctionTypeChecker
binaryOperator name = FunctionTypeChecker $
  \pos paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getPos e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2 || getType e2 == NullType) $
          addError pos [iii|expected types to be equals but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            getType e1,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError pos [iii|sum expects one argument|]
        pure (fmap snd tExprs, malo, (name, []))

comparisonFunction :: Text -> FunctionTypeChecker
comparisonFunction name = FunctionTypeChecker $
  \pos paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getPos e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2) $
          addError pos [iii|expected types to be equal but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            BooleanType,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError pos [iii|?? expects two parameters|]
        pure ([], UnknownType, undefined)

nativeFunctions :: Map Text FunctionTypeChecker
nativeFunctions =
  Map.fromList
    [ ("==", equalityFunction "=="),
      ("!=", equalityFunction "!="),
      ("+", binaryOperator "+"),
      ("*", binaryOperator "*"),
      ("+", binaryOperator "+"),
      ( "-", binaryOperator "-"),
      ("/", binaryOperator "/"),
      ("%", binaryOperator "%"),
      ("&&", binaryOperator "&&"),
      ("<", comparisonFunction "<"),
      (">", comparisonFunction ">"),
      ("<=", comparisonFunction "<="),
      (">=", comparisonFunction ">="),
      ( "char_length",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              unless (getType e1 `matchesType` StringType) $
                addError pos [iii|expected types to be equal but #{getType e1} is not equal to StringType|]
              pure
                ( [e1],
                  IntType,
                  ("char_length", [getType e1])
                )
            _ -> do
              addError pos [iii|?? expects two parameters|]
              pure ([], IntType, undefined)
      ),
      ( "??",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1), (_p2, e2)] -> do
              unless (getType e1 `matchesType` getType e2) $
                addError pos [iii|expected types to be equal but #{getType e1} is not equal to #{getType e2}|]
              pure
                ( [e1, e2],
                  getType e1,
                  ("??", [getType e1, getType e2])
                )
            _ -> do
              addError pos [iii|?? expects two parameters|]
              pure ([], IntType, undefined)
      )
      , ( "max",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("max", [getType e1]))
            _ -> do
              addError pos [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "!",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], BooleanType, ("!", [getType e1]))
            _ -> do
              addError pos [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "sum",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("sum", [getType e1]))
            _ -> do
              addError pos [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "sum_if",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1), (_p2, e2)] -> do
              pure ([e1, e2], IntType, ("sum_if", [getType e1, getType e2]))
            _ -> do
              addError pos [iii|sum_if expects two parameter|]
              pure ([], IntType, undefined)
      )
      , ( "extract_year",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("extract_month", [getType e1]))
            _ -> do
              addError pos [iii|extract_year needs one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "extract_month",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("extract_month", [getType e1]))
            _ -> do
              addError pos [iii|extract_month needs one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "count_distinct",
        FunctionTypeChecker $ \pos paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getPos e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("count_distinct", [getType e1]))
            _ -> do
              addError pos [iii|count_distinct needs one parameter|]
              pure ([], IntType, undefined)
      )
      -- , ("avg", FunctionType RuntimeFunction
      --     { rFuncName = "avg"
      --     , rFuncNotation = DefaultNotation
      --     , rFuncCheckTypes = \pos paramExprs -> do
      --         tExprs <- forM paramExprs $ \e -> do
      --           (getPos e,) <$> compileExpression e
      --         case tExprs of
      --           [(_p, e)] -> do
      --             unless (getType e `matchesType` IntType) $
      --               addError pos [iii|expected a number but got #{getType e}|]
      --             pure ([e], ValueMetadata IntType LiteralSource)
      --           _ -> do
      --             addError pos [iii|sum expects one argument|]
      --             pure ([], ValueMetadata IntType ErrorSource)
      --     }
      --   )
    ]

data FunctionNotation
  = InfixNotation
  | DefaultNotation
  deriving (Eq, Show)

resolveName :: (HasTypeInfo m) => (Text -> m ()) -> Text -> m (QueryExpression Compiled)
resolveName addError' name = do
  TypeInfo {..} <- getTypeInfo
  let inTopLevelDefs =
        variablesInScope
          & filter (\(n, _) -> n == name)

      inNativeFunctions =
        Map.lookup name nativeFunctions

  case (inTopLevelDefs, inNativeFunctions) of
    ([(_, val)], Nothing) ->
      pure val
    ([], Just f) ->
      pure $ ExprExt $ ComputedFunction (FunctionType f) f
    ([], Nothing) -> do
      addError' [iii|reference not found: '#{name}'|]
      pure $ ExprExt $ ExprCompilationFailed $ Lit (0, 0) $ LiteralString "mori"
    ((_, val) : _, _) ->
      pure val

addError :: (HasTypeInfo m) => (Int, Int) -> Text -> m ()
addError pos newError =
  modifyTypeInfo $ \ti ->
    ti
      { errors = ti.errors ++ [CompilerError newError pos]
      }

addCatastrophicError ::
  forall a m.
  (HasTypeInfo m, MonadError (QueryStatement Compiled, TypeInfo) m) =>
  Text ->
  (Int, Int) ->
  QueryStatement Compiled ->
  m a
addCatastrophicError newError position expr = do
  addError position newError
  ti <- getTypeInfo
  throwError (expr, ti)
