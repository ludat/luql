{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module LuQL.SqlGeneration where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader
import Control.Monad.State.Strict (StateT (runStateT), gets, modify')

import Data.Aeson (ToJSON (..))
import Data.Bifunctor qualified
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.String.Interpolate (__i, i, iii)
import Data.Text (Text)

import GHC.Generics

import LuQL.Compiler hiding (compileExpression, compileStatement)
import LuQL.Compiler qualified as T
import LuQL.Types qualified as T

data SqlExpression
  = LiteralString Text
  | LiteralInt Int
  | LiteralFloat Float
  | LiteralBoolean Bool
  | LiteralNull
  | Ref SqlIdentifier
  | Call FunctionNotation Text [SqlExpression]
  | RawSql [Either Text SqlExpression]
  deriving (Eq, Show, Generic, ToJSON)

newtype SqlIdentifier = SqlIdentifier { asText :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON)

data PartialQuery = PartialQuery
  { fromTable :: Maybe PartialFromTable
  , selectedColumns :: [SelectedColumn]
  , wheres :: [SqlExpression]
  , joins :: [(Table, [SqlExpression])]
  , groupBy :: Maybe GroupByDefinition
  , orderBy :: [(SqlExpression, Maybe T.OrderDirection)]
  }
  deriving (Eq, Show, Generic, ToJSON)

data PartialFromTable
  = SubqueryTable PartialQuery SqlIdentifier
  | LiteralTable Table
  deriving (Eq, Show, Generic, ToJSON)

data SelectedColumn
  = SelectNewColumn SqlExpression SqlIdentifier
  | SelectColumnFromTable SqlIdentifier
  | SelectFromTable SqlIdentifier
  deriving (Eq, Show, Generic, ToJSON)

data Table = FromTable
  { getTableName :: SqlIdentifier
  , getTableAlias :: SqlIdentifier
  , getTableColumns :: [(SqlIdentifier, SqlIdentifier)]
  } deriving (Eq, Show, Generic, ToJSON)

newtype GroupByDefinition = GroupByDefinition
  { byColumns :: [SqlExpression]
  } deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON)

class HasPartialQuery m where
  getPartialQuery :: m PartialQuery
  putPartialQuery :: PartialQuery -> m ()

type CompileM = StateT CompileState Identity

newtype CompileState = CompileState {compileStatePartialQuery :: PartialQuery}

instance HasPartialQuery CompileM where
  getPartialQuery :: CompileM PartialQuery
  getPartialQuery = do
    gets (.compileStatePartialQuery)
  putPartialQuery :: PartialQuery -> CompileM ()
  putPartialQuery partialQuery =
    modify' (\cs -> cs {compileStatePartialQuery = partialQuery})

modifyPartialQuery :: (Monad m, HasPartialQuery m) => (PartialQuery -> PartialQuery) -> m ()
modifyPartialQuery f = do
  q <- getPartialQuery
  putPartialQuery $ f q

compileStatement :: (Monad m, HasPartialQuery m) => T.QueryStatement Compiled -> m ()
compileStatement q@(T.From _ (modelName, model)) = do
  addNewSubqueryIfNecessary q
  modifyPartialQuery $
    \query ->
      query
        { fromTable =
            Just $
              LiteralTable $
                FromTable
                  (SqlIdentifier model.tableName)
                  (SqlIdentifier modelName)
                  (Map.keys model.columns
                  & fmap (\n -> (SqlIdentifier n, compileNameForTableColumn modelName n))
                  ),
          selectedColumns =
            query.selectedColumns ++ [SelectFromTable $ SqlIdentifier "t"],
          wheres = []
        }
compileStatement q@(T.Where _ expression) = do
  addNewSubqueryIfNecessary q
  let newWhere = compileExpression expression
  modifyPartialQuery $
    \query ->
      query
        { wheres = query.wheres ++ [newWhere]
        }
compileStatement q@(T.Join _ (modelName, model) expr) = do
  addNewSubqueryIfNecessary q
  let compExpr = compileExpression expr

  modifyPartialQuery $
    \query ->
      query
        { selectedColumns = query.selectedColumns ++ [SelectFromTable $ SqlIdentifier modelName],
          joins =
            query.joins
              ++ [ ( FromTable
                       (SqlIdentifier model.tableName)
                       (SqlIdentifier modelName)
                       (Map.keys model.columns
                       & fmap (\n -> (SqlIdentifier n, compileNameForTableColumn modelName n))
                       ),
                     [compExpr]
                   )
                 ]
        }
compileStatement query@(T.GroupBy _ groupByExpressions lets) = do
  addNewSubqueryIfNecessary query
  let newByColumns = fmap (Data.Bifunctor.first compileExpression) groupByExpressions
  let compiledLets =
        lets
          & fmap extractGroupByBody
          & fmap (\(t, q) -> (t, compileExpression q))
  modifyPartialQuery $ \partialQuery ->
    partialQuery
      { selectedColumns =
          (mapMaybe (groupBy2SelectColumn) (newByColumns))
            ++ fmap (\(nombre, expr) -> SelectNewColumn expr $ SqlIdentifier nombre) compiledLets,
        groupBy = Just $ GroupByDefinition $ fmap fst newByColumns
      }
  where
    groupBy2SelectColumn :: (SqlExpression, Maybe Text) -> Maybe SelectedColumn
    groupBy2SelectColumn (Ref name, Nothing) = Just $ SelectColumnFromTable name
    groupBy2SelectColumn (expr, Just name) = Just $ SelectNewColumn expr $ SqlIdentifier name
    groupBy2SelectColumn (_expr, _name) = Nothing

    extractGroupByBody :: T.QueryStatement a -> (T.ColumnName, T.QueryExpression a)
    extractGroupByBody (T.Let _ letName value) = (letName, value)
    extractGroupByBody _qe = error [iii|no vale poner algo que no sea let en groupby|]
compileStatement q@(T.Let _ti name expr) = do
  addNewSubqueryIfNecessary q
  modifyPartialQuery $ \query ->
    query
      { selectedColumns = query.selectedColumns ++ [SelectNewColumn (compileExpression expr) $ SqlIdentifier name]
      }
compileStatement q@(T.OrderBy _ exprs) = do
  addNewSubqueryIfNecessary q

  let orderByExprs = fmap (\(expr, dir) -> (compileExpression expr, dir)) exprs

  modifyPartialQuery $ \query ->
    query
      { orderBy = orderByExprs
      }
compileStatement s@(T.Return () expressions) = do
  addNewSubqueryIfNecessary s
  modifyPartialQuery $
    \query ->
      query
        { selectedColumns =
            expressions
            & mapMaybe (\case
                (T.ExprExt (ComputedColumn _ (ColumnWithTable table name))) ->
                  Just $ SelectNewColumn (Ref $ compileNameForTableColumn table name) (compileNameForTableColumn table name)
                (T.ExprExt (ComputedColumn _ (ColumnWithoutTable name))) ->
                  Just $ SelectNewColumn (Ref $ SqlIdentifier name) (SqlIdentifier name)
              )
        }
compileNameForTableColumn :: Text -> Text -> SqlIdentifier
compileNameForTableColumn tableName columnName =
  SqlIdentifier (tableName <> "." <> columnName)

addNewSubqueryIfNecessary :: (Monad m, HasPartialQuery m) => T.QueryStatement Compiled -> m ()
addNewSubqueryIfNecessary q = do
  pq <- getPartialQuery
  when (isNewSubqueryIfNecessary q pq) $
    modifyPartialQuery $ \partialQuery ->
      PartialQuery
        { fromTable = Just $ SubqueryTable partialQuery (SqlIdentifier "t"),
          selectedColumns = [SelectFromTable $ SqlIdentifier "t"],
          wheres = [],
          joins = [],
          groupBy = Nothing,
          orderBy = []
        }

isNewSubqueryIfNecessary :: T.QueryStatement Compiled -> PartialQuery -> Bool
isNewSubqueryIfNecessary expr pq =
  case expr of
    (T.From _ _) -> False
    (T.Where _ _) -> thereIsAGroupBy || thereAreOtherLets
    (T.Join _ _ _) -> thereIsAGroupBy
    -- Aca podria optimizar porque si el nuevo let no usa los anteriores
    -- esta todo bien
    (T.Let _ _ _) -> thereIsAGroupBy || thereAreOtherLets
    (T.Return _ _) -> thereAreOtherLets
    (T.GroupBy _ _ _) -> thereIsAGroupBy || thereAreOtherLets || thereIsAJoin
    (T.OrderBy _ _) -> False
  where
    thereIsAGroupBy =
      pq.groupBy
        & isJust
    thereIsAJoin =
      length pq.joins > 0
    thereAreOtherLets =
      pq.selectedColumns
        & any
          ( \case
              SelectNewColumn _ _ -> True
              _ -> False
          )

compileExpression :: T.QueryExpression Compiled -> SqlExpression
compileExpression (T.Lit _ (T.LiteralString t)) = LiteralString t
compileExpression (T.Lit _ (T.LiteralInt n)) = LiteralInt n
compileExpression (T.Lit _ (T.LiteralFloat n)) = LiteralFloat n
compileExpression (T.Lit _ (T.LiteralBoolean b)) = LiteralBoolean b
compileExpression (T.Lit _ (T.LiteralNull)) = LiteralNull
compileExpression (T.Apply _ ((name, paramTypes)) params) =
  let compiledParams = fmap compileExpression params
   in case (name, compiledParams, paramTypes) of
        ("==", [v1, _null], [_, NullType]) -> RawSql [Right v1, Left [i| IS NULL|]]
        ("==", [_, _], [_, _]) -> Call InfixNotation "=" compiledParams
        ("!=", [v1, _null], [_, NullType]) -> RawSql [Right v1, Left [i| IS NOT NULL|]]
        ("!=", [_, _], [_, _]) -> Call InfixNotation "<>" compiledParams
        ("-", [v1, v2], [DateType, DateType]) -> RawSql [Left "(", Right v1, Left " - ", Right v2, Left ")", Left [i| * interval '1' day|]]
        ("-", [_, _], [_, _]) -> Call InfixNotation "-" compiledParams
        ("/", [_, _], [_, _]) -> Call InfixNotation "/" compiledParams
        ("*", [_, _], [_, _]) -> Call InfixNotation "*" compiledParams
        ("+", [_, _], [_, _]) -> Call InfixNotation "+" compiledParams
        ("%", [_, _], [IntType, IntType]) -> Call InfixNotation "%" compiledParams
        ("&&", [_, _], [BooleanType, BooleanType]) -> Call InfixNotation "and" compiledParams
        ("<", _, _) -> Call InfixNotation "<" compiledParams
        (">", _, _) -> Call InfixNotation ">" compiledParams
        ("<=", _, _) -> Call InfixNotation "<=" compiledParams
        (">=", _, _) -> Call InfixNotation ">=" compiledParams
        ("char_length", [_], [_]) -> Call DefaultNotation "char_length" compiledParams
        ("date_between", [d, s1, s2], [DateType, StringType, StringType]) ->
          RawSql [Right d, Left " BETWEEN ", Right s1, Left " AND ", Right s2]
        ("extract_month", [val], [DateType]) ->
          RawSql [Left "extract(month from ", Right val, Left ")"]
        ("extract_year", [val], [DateType]) ->
          RawSql [Left "extract(year from ", Right val, Left ")"]
        ("max", [_], [_]) -> Call DefaultNotation "max" compiledParams
        ("sum", [_], [_]) -> Call DefaultNotation "sum" compiledParams
        ("sum_if", [cond, val], [BooleanType, _]) ->
          RawSql [Left "sum(CASE WHEN ", Right cond, Left " THEN ", Right val, Left " ELSE 0 END)"]
        ("count_distinct", [val], [_]) ->
          RawSql [Left "count(distinct", Right val, Left ")"]
        ("in", [val, list], [_, _]) -> -- TODO: Typecheck this
          RawSql [Right val, Left " IN ", Right list]
        ("!", [val], [BooleanType]) ->
          RawSql [Left "NOT ", Right val]
        ("??", [_, _], [_, _]) ->
          Call DefaultNotation "COALESCE" compiledParams
        -- ("pg_typeof", [_], [_]) -> Call DefaultNotation "pg_typeof" compiledParams
        _ ->
          error
            [__i|unknown function: #{name} con
              #{compiledParams}
              y
              #{paramTypes}
            |]
compileExpression qe@(T.Ref _ _) = do
  error [iii|compileExpression #{qe}|]
compileExpression (T.RawSql _ cosos) =
  RawSql $ fmap (fmap compileExpression) cosos
compileExpression (T.If _ condExpr thenExpr elseExpr) =
  let compCondExpr = compileExpression condExpr
      compThenExpr = compileExpression thenExpr
      compElseExpr = compileExpression elseExpr
   in RawSql
        [ Left
            [i|
  CASE
    WHEN |],
          Right compCondExpr,
          Left [i| THEN |],
          Right compThenExpr,
          Left
            [i|
    ELSE |],
          Right compElseExpr,
          Left
            [i|
  END
    |]
        ]
compileExpression (T.ExprExt (ComputedColumn _ (ColumnWithTable table name))) =
  Ref $ compileNameForTableColumn table name
compileExpression (T.ExprExt (ComputedColumn _ (ColumnWithoutTable name))) =
  Ref $ SqlIdentifier name
compileExpression (T.ExprExt (ComputedModel _ _ _ _)) =
  error "falle"
compileExpression (T.ExprExt (ComputedModelDefinition _ _)) =
  error "falle"
compileExpression (T.ExprExt (ComputedFunction _ _)) =
  error "falle"
compileExpression (T.ExprExt (ExprCompilationFailed _)) =
  error "falle"

compileStatements :: [T.QueryStatement Compiled] -> PartialQuery
compileStatements compiledStatements =
  forM_ compiledStatements compileStatement
    & ( `runStateT`
          CompileState
            { compileStatePartialQuery =
                PartialQuery
                  { fromTable = Nothing,
                    wheres = [],
                    selectedColumns = [],
                    joins = [],
                    groupBy = Nothing,
                    orderBy = []
                  }
            }
      )
    & runIdentity
    & snd
    & (.compileStatePartialQuery)
