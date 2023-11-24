{-# LANGUAGE DeriveAnyClass #-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module LuQL.Compiler where

import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State.Strict (StateT (..), get, gets, modify', put)
import Control.Monad.Validate

import Data.Aeson.Types
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate (i, iii)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import GHC.Generics (Generic)

import LuQL.Parser
import LuQL.Types

import Safe (headMay)

data Compiled

type instance StmtE _ "ctx" Compiled = ()

type instance StmtE "from" "model" Compiled = (Text, ModelDefinition)

type instance StmtE "join" "model" Compiled = (Text, ModelDefinition)

type instance StmtE "join" "on" Compiled = (QueryExpression Compiled)

type instance StmtE "ext" "ext" Compiled = Void

type instance ExprE "prop" "ctx" Compiled = Void

type instance ExprE "lit" "ctx" Compiled = RuntimeType

type instance ExprE "ref" "ctx" Compiled = RuntimeType

type instance ExprE "apply" "ctx" Compiled = RuntimeType

type instance ExprE "apply" "function" Compiled = (Text, [RuntimeType])

type instance ExprE "raw" "ctx" Compiled = RuntimeType

type instance ExprE "if" "ctx" Compiled = RuntimeType

type instance ExprE "computed" "ctx" Compiled = RuntimeType

type instance ExprE "ext" "ext" Compiled = ExtExprCompiled

data ExtExprCompiled
  = ExprCompilationFailed (QueryExpression Raw)
  | ComputedColumn (ExprE "computed" "ctx" Compiled) ColumnDefinition
  | ComputedModel (ExprE "computed" "ctx" Compiled) Text Text (Map TableName (ColumnName, ColumnName))
  | ComputedModelDefinition (ExprE "computed" "ctx" Compiled) ModelDefinition
  | ComputedFunction (ExprE "computed" "ctx" Compiled) FunctionTypeChecker
  deriving (Generic)

instance ToJSON ExtExprCompiled

instance ToJSON (QueryStatement Compiled)
instance ToJSON (QueryExpression Compiled)

deriving instance
  ( Show (ExprE "computed" "ctx" Compiled)
  ) =>
  Show ExtExprCompiled

deriving instance
  ( Eq (ExprE "computed" "ctx" Compiled)
  ) =>
  Eq ExtExprCompiled

type TableName = Text
type ColumnName = Text

data ModelDefinition = ModelDefinition
  { tableName :: TableName
  , defaultSingularName :: Text
  , columns :: Map ColumnName RuntimeType
  , implicitWhere :: Maybe (QueryExpression Raw)
    -- this expression need the table name to be replaced
  , relatedTables :: Map TableName (ColumnName, ColumnName)
  } deriving (Eq, Show, Generic, ToJSON)

instantiateModel :: ModelDefinition -> RuntimeType
instantiateModel model =
  fst $ instantiateModel' model Nothing

instantiateModel' :: ModelDefinition -> Maybe Text -> (RuntimeType, Text)
instantiateModel' model (maybeModelName) =
  let type_ = (ModelType model.columns)
      modelName = fromMaybe model.defaultSingularName maybeModelName
   in (type_, modelName)

data Error
  = CompilerError Text Range
  deriving (Eq, Show, Generic, ToJSON)

data CompiledQuery = CompiledQuery
  { compiledStatements :: [QueryStatement Compiled]
  , resultColumns :: [Text]
  , graph :: Maybe DrawStrategy
  }
  deriving (Show, Eq, Generic)

instance ToJSON CompiledQuery

type Models = Map Text ModelDefinition

data BarChartDefinition = BarChartDefinition
  { xAxis :: Text
  , yAxes :: [Text]
  } deriving (Show, Eq, Generic, ToJSON)

data SingleValueGaugeDefinition = SingleValueGaugeDefinition
  { column :: Text
  } deriving (Show, Eq, Generic, ToJSON)

data DrawStrategy
  = BarChart BarChartDefinition
  | SingleValueGauge SingleValueGaugeDefinition
  deriving (Show, Eq, Generic, ToJSON)

compileProgram :: Models -> RawQuery -> Either [Error] CompiledQuery
compileProgram models rawQuery =
  let compilerState = compileProgram' models Nothing rawQuery
   in if null compilerState.errors
        then
          let
            newColumns =
              compilerState.typeInfo.variablesInScope
              & mapMaybe (\(name, value) ->
                  case value of
                    (ExprExt (ComputedColumn _ _)) -> Just [name]
                    (ExprExt (ComputedModel (ModelType cols) _ _ _)) ->
                        cols
                        & Map.toList
                        & fmap (\(n, _type) -> [i|#{name}.#{n}|])
                        & Just
                    _ -> Nothing
                )
              & mconcat

          in Right $ CompiledQuery
            { compiledStatements = compilerState.emitedCompiledStatements
            , resultColumns = newColumns
            , graph = compilerState.graphInfo
            }
        else Left compilerState.errors

compileProgram' :: Models -> Maybe Position -> RawQuery -> CompilerState
compileProgram' models mayPosition (RawQuery qss) =
  compileStatements qss
        & runValidateT
        & ( `runStateT`
              ( CompilerState
                { typeInfo = TypeInfo
                  { variablesInScope = Map.toList $
                            fmap
                              ( \m ->
                                  ExprExt $ ComputedModelDefinition (ModelDefinitionType) m
                              )
                              models
                  }
                , errors = []
                , completionIntent = mayPosition
                , completionResult = []
                , emitedCompiledStatements = []
                , graphInfo = Nothing
                }
              )
          )
        & runIdentity
        & snd

data Completion = Completion
  { newText :: Text
  , from :: Position
  } deriving (Show, Eq, Generic, ToJSON)

generateCompletions :: Position -> Models -> RawQuery -> [Completion]
generateCompletions position models rawQuery =
  (compileProgram' models (Just position) rawQuery).completionResult

compileStatements :: [QueryStatement Raw] -> CompilerM ()
compileStatements qss = do
  forM_ qss $ \qs -> do
    void $ tolerate $ compileStatement qs

data CompilerState = CompilerState
  { typeInfo :: TypeInfo
  , completionIntent :: Maybe Position
  , completionResult :: [Completion]
  , emitedCompiledStatements :: [QueryStatement Compiled]
  , errors :: [Error]
  , graphInfo :: Maybe DrawStrategy
  } deriving (Show, Eq, Generic, ToJSON)

type CompilerM = ValidateT () (StateT CompilerState Identity)

emit :: QueryStatement Compiled -> CompilerM ()
emit queryStatement = do
  modify' $ \compilerState ->
    compilerState {
      emitedCompiledStatements = compilerState.emitedCompiledStatements ++ [queryStatement]
    }

compileStatement :: QueryStatement Raw -> CompilerM ()
compileStatement (From srcRange (maybeModelAs, modelExpr)) = do
  tcModel <- compileExpression modelExpr
  case tcModel of
    (ExprExt (ComputedModelDefinition _ model)) -> do
      let (runtimeModel, modelName) = instantiateModel' model maybeModelAs
      modifyTypeInfo $ \typeInfo ->
        typeInfo
          { variablesInScope =
              typeInfo.variablesInScope
                ++ [(modelName, ExprExt $ ComputedModel runtimeModel modelName (model.tableName) model.relatedTables)]
          }
      emit $ From () (modelName, model)
      case model.implicitWhere of
        Just implicitWhere -> compileStatement $ Where srcRange $ replaceExpression (Ref nullRange "self") (Ref nullRange modelName) implicitWhere
        Nothing -> pure ()
    _ -> do
      addError srcRange [iii|Invalid model|]
compileStatement (Where srcRange expression) = do
  newWhere <- compileExpression expression
  unless (getType newWhere `matchesType` BooleanType) $ do
    addError srcRange [iii|wrong type, expected boolean|]

  emit $ Where () newWhere
compileStatement (Join srcRange (maybeModelAs, modelExpr) mayJoinExpr) = do
  originalTypeInfo <- getTypeInfo

  otherModelExpr <- compileExpression modelExpr
  newModelToJoin <- case otherModelExpr of
    (ExprExt (ComputedModelDefinition _ modelDefinition)) ->
      pure modelDefinition
    _ ->
      addCatastrophicError
        [iii|value to join is not a model|]
        srcRange

  let (otherRuntimeModel, otherModelName) = instantiateModel' newModelToJoin maybeModelAs

  modifyTypeInfo $ \typeInfo ->
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
                    srcRange
              ) pure


        case newModelToJoin.implicitWhere of
          Just where_ ->
            compileExpression $
              Apply
                srcRange
                (Ref srcRange "&&")
                [ Apply
                    srcRange
                    (Ref srcRange "==")
                    [ Prop srcRange (Ref srcRange modelToJoin) otherColumn,
                      Prop srcRange (Ref srcRange otherModelName) myColumn
                    ],
                  replaceExpression (Ref nullRange "self") (Ref nullRange otherModelName) where_
                ]
          Nothing ->
            compileExpression $
              Apply
                srcRange
                (Ref srcRange "==")
                [ Prop srcRange (Ref srcRange modelToJoin) otherColumn,
                  Prop srcRange (Ref srcRange otherModelName) myColumn
                ]

  emit $ Join () (otherModelName, newModelToJoin) joinExpression
compileStatement (GroupBy _ groupByColumns letStatements) = do
  _ <- compileAndSetTheNewTypeInfo Tag groupByColumns
  originalCompilerState <- get
  put originalCompilerState { emitedCompiledStatements = [] }
  _ <- tolerate $ mapM compileStatementAndTag letStatements
  afterDefinitionsCompilerState <- get
  put afterDefinitionsCompilerState { emitedCompiledStatements = originalCompilerState.emitedCompiledStatements }
  tg <- compileAndSetTheNewTypeInfo Discard groupByColumns
  emit $ GroupBy () tg afterDefinitionsCompilerState.emitedCompiledStatements
  where
    compileStatementAndTag stmt = do
      case stmt of
        Let _ name _ -> do
          ret <- compileStatement stmt
          modifyTypeInfo $ \ty ->
            ty {
              variablesInScope = ty.variablesInScope
                & fmap (\(n, v) -> if n /= name
                  then (n, v)
                  else case v of
                    ExprExt (ComputedColumn t columnDefinition) ->
                      (n, ExprExt $ ComputedColumn (KeptByGroupBy t) columnDefinition)
                    _ ->
                      (n, v)
                  )
            }
          pure ret
        _ ->
          compileStatement stmt
    compileAndSetTheNewTypeInfo tagOrDiscard columns = do
      oldTypeInfo <- getTypeInfo
      compiledColumns <- forM columns $ \(column, name) -> do
        compiledExpression <- compileExpression column
        pure (compiledExpression, name)

      vars <- foldM (\acc (name, expression) -> do
        case expression of
          ExprExt (ComputedModel (ModelType propsMap) modelName tableName relatedTables) -> do
            let relevantColumns =
                  compiledColumns
                  & mapMaybe (\case
                    (ExprExt (ComputedColumn _ (ColumnWithTable t n)), _) | t == modelName -> Just n
                    _ -> Nothing
                    )
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
                    (ExprExt (ComputedColumn _ (ColumnWithoutTable n)), _) -> Just n
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

compileStatement (Let _srcRange name expression) = do
  e <- compileExpression expression

  modifyTypeInfo $ \ti ->
    ti
      { variablesInScope =
          ti.variablesInScope
            ++ [ ( name,
                   ExprExt $ ComputedColumn (getType e) (ColumnWithoutTable name)
                 )
               ]
      }

  emit $ Let () name e
compileStatement (Return _ exprs) = do
  tExprs <- mapM compileExpression exprs

  oldTypeInfo <- getTypeInfo
  compiledColumns <- forM exprs $ \(column) -> do
    compiledExpression <- compileExpression column
    pure (compiledExpression)

  vars <- foldM (\acc (name, expression) -> do
    case expression of
      ExprExt (ComputedModel (ModelType propsMap) modelName tableName relatedTables) -> do
        let relevantColumns =
              compiledColumns
              & mapMaybe (\case
                (ExprExt (ComputedColumn _ (ColumnWithTable t n))) | t == modelName -> Just n
                _ -> Nothing
                )
        pure $ acc ++
          [ (name, ExprExt (ComputedModel
              (ModelType (propsMap & Map.mapMaybeWithKey (\k v ->
                  if k `elem` relevantColumns
                      then Just $ v
                      else Nothing
                )))
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
                (ExprExt (ComputedColumn _ (ColumnWithoutTable n))) -> Just n
                _ -> Nothing
                )
        in
          if name `elem` relevantColumns
            then pure $ acc ++ [(name, ExprExt $ ComputedColumn t columnDefinition)]
            else pure acc

      _ -> pure acc
    )
    ([] :: [(Text, QueryExpression Compiled)])
    oldTypeInfo.variablesInScope

  modifyTypeInfo $ \ty ->
    ty {
      variablesInScope = vars
    }
  emit $ Return () tExprs
compileStatement (OrderBy _ exprs) = do
  tExprs <- forM exprs $ \(expr, dir) -> do
    compiledExpr <- compileExpression expr
    pure (compiledExpr, dir)
  emit $ OrderBy () tExprs
compileStatement (StmtExt (ExtStmtInvalid srcRange t)) = do
  addCatastrophicError [iii|Invalid statement: #{t}|] srcRange
compileStatement (StmtExt (ExtStmtEmptyLine srcRange)) = do
  completeWith srcRange $ \_complPos -> do
    pure
      [ Completion "from" srcRange.begin
      , Completion "where" srcRange.begin
      , Completion "group by" srcRange.begin
      , Completion "order by" srcRange.begin
      , Completion "let" srcRange.begin
      ]
compileStatement (StmtExt (ExtStmtBarChart _ xExprRaw yExprsRaw) ) = do
  xAxis <- compileExpression xExprRaw >>= getColumnAsText (getSrcRange xExprRaw)
  yAxes <- forM yExprsRaw $ \yExprRaw -> compileExpression yExprRaw >>= getColumnAsText (getSrcRange yExprRaw)
  modify' $ \cs ->
    cs {
      graphInfo = Just $ BarChart $ BarChartDefinition
        { xAxis = xAxis
        , yAxes = yAxes
        }
    }
  where
    getColumnAsText :: Range -> QueryExpression Compiled -> CompilerM Text
    getColumnAsText srcRange expr =
      case expr of
        (ExprExt (ComputedColumn _ (ColumnWithTable t n))) -> pure [i|#{t}.#{n}|]
        (ExprExt (ComputedColumn _ (ColumnWithoutTable n))) -> pure n
        (_) -> addCatastrophicError [i|must be a literal column to be used in here|] srcRange



replaceExpression :: QueryExpression Raw -> QueryExpression Raw -> QueryExpression Raw -> QueryExpression Raw
replaceExpression oldExpression newExpression expressionToReplace =
  if oldExpression == expressionToReplace
    then newExpression
    else case expressionToReplace of
      Apply ctx func params ->
        Apply ctx (go func) $ fmap go params
      If ctx condExpr thenExpr elseExpr ->
        If ctx (go condExpr) (go thenExpr) (go elseExpr)
      RawSql ctx exprs ->
        RawSql ctx (exprs & fmap (fmap go))
      Prop ctx expr name ->
        Prop ctx (go expr) name
      ExprExt (ExtExprEmptyExpr ctx) ->
        ExprExt (ExtExprEmptyExpr ctx)
      Ref ctx name ->
        Ref ctx name
      Lit ctx value ->
        Lit ctx value
  where
    go = replaceExpression oldExpression newExpression

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

data ColumnDefinition
  = ColumnWithTable Text Text
  | ColumnWithoutTable Text
   deriving (Show, Eq, Generic, ToJSON)

data RuntimeFunction = RuntimeFunction
  { rFuncNotation :: FunctionNotation,
    rFuncName :: Text,
    rFuncCheckTypes :: Range -> [QueryExpression Raw] -> CompilerM ([QueryExpression Compiled], RuntimeType)
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
  | ModelType (Map Text RuntimeType)
  | ModelDefinitionType
  | FunctionType FunctionTypeChecker
  | KeptByGroupBy RuntimeType
  deriving (Eq, Show, Generic, ToJSON)

newtype FunctionTypeChecker
  = FunctionTypeChecker
      ( Range ->
        [QueryExpression Raw] ->
        CompilerM ([QueryExpression Compiled], RuntimeType, (Text, [RuntimeType]))
      )

instance ToJSON FunctionTypeChecker where
  toJSON _ = String "function"

instance Show FunctionTypeChecker where
  show _ = "function"

instance Eq FunctionTypeChecker where
  _ == _ = False

data TypeInfo = TypeInfo
  { variablesInScope :: [(Text, QueryExpression Compiled)]
  } deriving (Show, Eq, Generic, ToJSON)

type TypeCheckM =
  StateT TypeInfo Identity

instance HasTypeInfo CompilerM where
  getTypeInfo =
    gets (.typeInfo)
  putTypeInfo newTypeInfo =
    modify' $ \compilerState -> compilerState { typeInfo = newTypeInfo }

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

compileExpression :: QueryExpression Raw -> CompilerM (QueryExpression Compiled)
compileExpression (Lit _ (LiteralString t)) = pure $ Lit (lit StringType) $ LiteralString t
compileExpression (Lit _ (LiteralInt n)) = pure $ Lit (lit IntType) $ LiteralInt n
compileExpression (Lit _ (LiteralFloat n)) = pure $ Lit (lit FloatType) $ LiteralFloat n
compileExpression (Lit _ (LiteralBoolean b)) = pure $ Lit (lit BooleanType) $ LiteralBoolean b
compileExpression (Lit _ (LiteralNull)) = pure $ Lit (lit NullType) LiteralNull
compileExpression expression@(Prop srcRange expr propName) = do
  tExpr <- compileExpression expr
  case tExpr of
    (ExprExt (ComputedModel (ModelType m) n _ _)) -> case Map.lookup propName m of
      Nothing -> do
        addError srcRange [iii|#{expr} no tiene una propiedad: #{propName}|]
        completeWith srcRange $ \completionPosition -> do
          let stringUntilComplPos = propName & T.take (completionPosition - srcRange.begin)
          m
            & Map.toList
            & filter (\(prop, _) -> T.isPrefixOf stringUntilComplPos prop)
            & fmap (\(prop, _) -> Completion { newText = prop, from = srcRange.begin + 1 })
            & pure
        pure $ ExprExt $ ExprCompilationFailed expression
      Just ty -> do
        pure $
          ExprExt $
            ComputedColumn ty (ColumnWithTable n propName)
    (ExprExt (ComputedModelDefinition _ _)) ->
      pure $ RawSql AnyType [Left "select centros.id from centro_de_costos centros left join centro_de_costos subcentros on centros.sub_centro_de_id=subcentros.id where centros.id= 227 or (centros.sub_centro_de_id = 227) or (subcentros.sub_centro_de_id = 227)"]
    -- -- TODO podria definirle propiedades a cosas que no sean tablas :thinking:
    _ -> case (getType tExpr, propName) of
      -- (_, "pg_type") -> do
      --   compileExpression
      --     ( Apply
      --         (0, 0)
      --         (Ref (0, 0) "pg_typeof")
      --         [ expr
      --         ]
      --     )
      (IntType, "even") -> do
        compileExpression
          ( Apply
              nullRange
              (Ref nullRange "==")
              [ Apply nullRange (Ref nullRange "%") [expr, Lit nullRange $ LiteralInt 2],
                Lit nullRange $ LiteralInt 0
              ]
          )
      (IntType, "in") -> do
        let functionTypeChecker = FunctionTypeChecker $ \_ paramExprs -> do
              tExprs <- forM paramExprs $ \e -> do
                (getSrcRange e,) <$> compileExpression e
              case tExprs of
                [(_p1, e1)] -> do
                  pure ([tExpr, e1], BooleanType, ("in", [getType tExpr, getType e1]))
                _ -> do
                  addError srcRange [iii|max expects one parameter|]
                  pure ([], IntType, undefined)
        pure $ ExprExt $ ComputedFunction (FunctionType functionTypeChecker) functionTypeChecker
      (IntType, _) -> do
        addError srcRange [iii|Int doesn't have prop '#{propName}'|]
        completeWith srcRange $ \_complPos -> pure
          [ Completion { newText = "in", from = srcRange.begin }
          , Completion { newText = "even", from = srcRange.begin }
          ]
        pure $ ExprExt $ ExprCompilationFailed expression
      (StringType, "length") -> do
        compileExpression
          ( Apply
              nullRange
              (Ref nullRange "char_length")
              [ expr
              ]
          )
      (StringType, _) -> do
        addError srcRange [iii|Int doesn't have prop '#{propName}'|]
        completeWith srcRange $ \_complPos -> pure
          [ Completion { newText = "length", from = srcRange.begin }
          ]
        pure $ ExprExt $ ExprCompilationFailed expression
      (DateType, "between") -> do
        let functionTypeChecker = FunctionTypeChecker $ \_ paramExprs -> do
              tExprs <- forM paramExprs $ \e -> do
                (getSrcRange e,) <$> compileExpression e
              case tExprs of
                [(p1, e1), (p2, e2)] -> do
                  unless (getType e1 `matchesType` StringType) $
                    addError p1 [iii|expected type is String but got #{getType e1}|]
                  unless (getType e2 `matchesType` StringType) $
                    addError p2 [iii|expected type is String but got #{getType e2}|]
                  pure ([tExpr, e1, e2], BooleanType, ("date_between", [getType tExpr, getType e1, getType e2]))
                _ -> do
                  addError srcRange [iii|max expects one parameter|]
                  pure ([], IntType, undefined)
        pure $ ExprExt $ ComputedFunction (FunctionType functionTypeChecker) functionTypeChecker
      (DateType, "year") -> do
        compileExpression
          ( Apply
              nullRange
              (Ref nullRange "extract_year")
              [ expr
              ]
          )
      (DateType, "month") -> do
        compileExpression
          ( Apply
              nullRange
              (Ref nullRange "extract_month")
              [ expr
              ]
          )
      (DateType, _) -> do
        addError srcRange [iii|Int doesn't have prop '#{propName}'|]
        completeWith srcRange $ \_complPos -> pure
          [ Completion "between" srcRange.begin
          , Completion "year" srcRange.begin
          , Completion "month" srcRange.begin
          ]
        pure $ ExprExt $ ExprCompilationFailed expression
      _ -> do
        addError srcRange [iii|1 eso no tiene una propiedad: #{getType tExpr}|]
        pure $
          ExprExt $
            ExprCompilationFailed expression
compileExpression (Ref srcRange name) = do
  resolveName srcRange name
compileExpression (Apply srcRange function params) = do
  typecheckedFunction <- compileExpression function
  (typecheckedParams, returnType, resolvedFunction) <- case typecheckedFunction of
    (ExprExt (ComputedFunction _ (FunctionTypeChecker checkTypes))) ->
      checkTypes srcRange params
    value -> do
      addError (getSrcRange function) [iii|No se pueden aplicar #{value}|]
      tExprs <- mapM compileExpression params
      pure (tExprs, malo, ("unknown", []))

  pure $ Apply returnType resolvedFunction typecheckedParams
compileExpression (RawSql _ exprOrText) = do
  coso <- mapM (mapM compileExpression) exprOrText
  pure $ RawSql AnyType coso
compileExpression (If _srcRange condExpr thenExpr elseExpr) = do
  tcCondExpr <- compileExpression condExpr
  tcThenExpr <- compileExpression thenExpr
  tcElseExpr <- compileExpression elseExpr

  unless (getType tcCondExpr `matchesType` BooleanType) $
    addError (getSrcRange condExpr) [iii|La condicion de un if deberia ser un booleano pero es #{getType tcCondExpr}|]

  unless (getType tcThenExpr `matchesType` getType tcElseExpr) $
    addError (getSrcRange elseExpr) [iii|El else y el then deberian matchear pero #{getType tcThenExpr} es diferente que #{getType tcElseExpr}|]

  pure $ If (getType tcThenExpr) tcCondExpr tcThenExpr tcElseExpr
compileExpression (ExprExt (ExtExprEmptyExpr srcRange)) = do
  completeAllLocalVars srcRange
  addCatastrophicError [iii|Missing expression|] srcRange

completeWith :: Range -> (Int -> CompilerM [Completion]) -> CompilerM ()
completeWith range f =
  gets (.completionIntent) >>= \case
    Just complPos | range.begin <= complPos && complPos <= range.end -> do
      completions <- f complPos
      modify' $ \cs -> cs
        { completionResult = completions
        }
    _ -> pure ()

completeAllLocalVars :: Range -> CompilerM ()
completeAllLocalVars range =
  completeWith range $ \complPos -> do
    ty <- getTypeInfo
    ty.variablesInScope
      & fmap (\(name, _) -> Completion { newText = name, from = complPos })
      & pure

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
  \srcRange paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getSrcRange e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2 || getType e2 == NullType) $
          addError srcRange [iii|expected types to be equals but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            BooleanType,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError srcRange [iii|sum expects one argument|]
        pure (fmap snd tExprs, malo, (name, []))

binaryOperator :: Text -> FunctionTypeChecker
binaryOperator name = FunctionTypeChecker $
  \srcRange paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getSrcRange e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2 || getType e2 == NullType) $
          addError srcRange [iii|expected types to be equals but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            getType e1,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError srcRange [iii|sum expects one argument|]
        pure (fmap snd tExprs, malo, (name, []))

comparisonFunction :: Text -> FunctionTypeChecker
comparisonFunction name = FunctionTypeChecker $
  \srcRange paramExprs -> do
    tExprs <- forM paramExprs $ \e -> do
      (getSrcRange e,) <$> compileExpression e
    case tExprs of
      [(_p1, e1), (_p2, e2)] -> do
        unless (getType e1 `matchesType` getType e2) $
          addError srcRange [iii|expected types to be equal but #{getType e1} is not equal to #{getType e2}|]
        pure
          ( [e1, e2],
            BooleanType,
            (name, [getType e1, getType e2])
          )
      _ -> do
        addError srcRange [iii|?? expects two parameters|]
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
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              unless (getType e1 `matchesType` StringType) $
                addError srcRange [iii|expected types to be equal but #{getType e1} is not equal to StringType|]
              pure
                ( [e1],
                  IntType,
                  ("char_length", [getType e1])
                )
            _ -> do
              addError srcRange [iii|?? expects two parameters|]
              pure ([], IntType, undefined)
      ),
      ( "??",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1), (_p2, e2)] -> do
              unless (getType e1 `matchesType` getType e2) $
                addError srcRange [iii|expected types to be equal but #{getType e1} is not equal to #{getType e2}|]
              pure
                ( [e1, e2],
                  getType e1,
                  ("??", [getType e1, getType e2])
                )
            _ -> do
              addError srcRange [iii|?? expects two parameters|]
              pure ([], IntType, undefined)
      )
      , ( "max",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("max", [getType e1]))
            _ -> do
              addError srcRange [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "!",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], BooleanType, ("!", [getType e1]))
            _ -> do
              addError srcRange [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "sum",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("sum", [getType e1]))
            _ -> do
              addError srcRange [iii|max expects one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "sum_if",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1), (_p2, e2)] -> do
              pure ([e1, e2], IntType, ("sum_if", [getType e1, getType e2]))
            _ -> do
              addError srcRange [iii|sum_if expects two parameter|]
              pure ([], IntType, undefined)
      )
      , ( "extract_year",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("extract_year", [getType e1]))
            _ -> do
              addError srcRange [iii|extract_year needs one parameter|]
              pure ([], IntType, undefined)
      )
      , ( "extract_month",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("extract_month", [getType e1]))
            _ -> do
              addError srcRange [iii|extract_month needs one parameter|]
              pure ([], IntType, undefined)
      )
      -- , ( "pg_typeof",
      --   FunctionTypeChecker $ \srcRange paramExprs -> do
      --     tExprs <- forM paramExprs $ \e -> do
      --       (getSrcRange e,) <$> compileExpression e
      --     case tExprs of
      --       [(_p1, e1)] -> do
      --         pure ([e1], StringType, ("pg_typeof", [getType e1]))
      --       _ -> do
      --         addError getSrcPos [iii|pg_typeof needs one parameter|]
      --         pure ([], StringType, undefined)
      -- )
      , ( "count_distinct",
        FunctionTypeChecker $ \srcRange paramExprs -> do
          tExprs <- forM paramExprs $ \e -> do
            (getSrcRange e,) <$> compileExpression e
          case tExprs of
            [(_p1, e1)] -> do
              pure ([e1], IntType, ("count_distinct", [getType e1]))
            _ -> do
              addError srcRange [iii|count_distinct needs one parameter|]
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
  deriving (Eq, Show, Generic, ToJSON)

resolveName :: Range -> Text -> CompilerM (QueryExpression Compiled)
resolveName srcRange name = do
  TypeInfo {..} <- getTypeInfo

  completeWith srcRange $ \complPos -> do
    let stringUntilComplPos =
          name & T.take (complPos - srcRange.begin)
    variablesInScope
      & filter (\(n, _) -> T.isPrefixOf stringUntilComplPos n)
      & fmap (\(n, _) -> Completion { newText = n, from = srcRange.begin })
      & pure

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
      addCatastrophicError [iii|reference not found: '#{name}'|] srcRange
    ((_, val) : _, _) ->
      pure val

addError :: Range -> Text -> CompilerM ()
addError srcRange newError =
  modify' $ \cs ->
    cs
      { errors = cs.errors ++ [CompilerError newError srcRange]
      }

addCatastrophicError ::
  forall a.
  Text ->
  Range ->
  CompilerM a
addCatastrophicError newError srcRange = do
  addError srcRange newError
  refute ()
