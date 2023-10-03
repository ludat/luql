{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LuQL.Render where

import Data.Function ((&))
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromJust)
import Data.String (fromString)
import Data.String.Interpolate (iii)
import Data.Text (Text, unpack)

import Database.PostgreSQL.Query (SqlBuilder)
import Database.PostgreSQL.Query qualified as PG
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Identifier (..), QualifiedIdentifier (..))

import LuQL.Compiler (FunctionNotation (..))
import LuQL.SqlGeneration
import LuQL.Types (OrderDirection (..))

renderPartialQuery :: PartialQuery -> SqlBuilder
renderPartialQuery PartialQuery {..} =
  let whereFragment = renderWheres wheres
      joinFragment = renderJoins joins
      groupByFragment = renderGroupBy groupBy
      orderByFragment = renderOrderBy orderBy
      otherFragments = catMaybes [joinFragment, whereFragment, groupByFragment, orderByFragment]
      fragments =
        if null otherFragments
          then mempty
          else [PG.sqlExp| ^{mconcat $ intersperse " " otherFragments}|]
   in [PG.sqlExp|SELECT ^{renderSelectedColumns selectedColumns}
    ^{renderFrom $ fromTable}^{fragments}|]

renderOrderBy :: [(SqlExpression, Maybe OrderDirection)] -> Maybe SqlBuilder
renderOrderBy [] = Nothing
renderOrderBy qs =
  let renderedColumns =
        qs
          & fmap (\(q, dir) -> [PG.sqlExp|^{renderExpr q}^{renderOrderDirection dir}|])
          & mconcat . intersperse ", "
   in Just [PG.sqlExp|ORDER BY ^{renderedColumns}|]

renderOrderDirection :: Maybe OrderDirection -> SqlBuilder
renderOrderDirection (Nothing) = mempty
renderOrderDirection (Just Asc) = [PG.sqlExp| ASC|]
renderOrderDirection (Just Desc) = [PG.sqlExp| DESC|]

renderFrom :: Maybe PartialFromTable -> SqlBuilder
renderFrom (Just (SubqueryTable fromPartialQuery aliasName)) =
  [PG.sqlExp|FROM (^{renderPartialQuery fromPartialQuery}) AS ^{aliasName}|]
renderFrom (Just (LiteralTable table)) =
  [PG.sqlExp|FROM (^{renderFromTable table}) AS "t"|]
renderFrom Nothing =
  [PG.sqlExp| |]

renderFromTable :: Table -> SqlBuilder
renderFromTable FromTable {..} =
  let columnRenames =
        getTableColumns
          & fmap
            ( \name ->
                [PG.sqlExp|^{qualifyIdentifier getTableName name} AS ^{renderNameForTableColumn getTableAlias name}|]
            )
          & mconcat . intersperse ", "
   in [PG.sqlExp|SELECT ^{columnRenames} FROM ^{getTableName}|]

renderGroupBy :: Maybe GroupByDefinition -> Maybe SqlBuilder
renderGroupBy Nothing = Nothing
renderGroupBy (Just GroupByDefinition {byColumns = []}) = Nothing
renderGroupBy (Just GroupByDefinition {byColumns = byColumns}) =
  let renderedColumns =
        byColumns
          & fmap (\q -> [PG.sqlExp|^{renderExpr q}|])
          & mconcat . intersperse ", "
   in Just [PG.sqlExp|GROUP BY ^{renderedColumns}|]

renderJoins :: [(Table, [SqlExpression])] -> Maybe SqlBuilder
renderJoins joins =
  let condList = fmap renderJoin joins
   in if null condList
        then Nothing
        else Just [PG.sqlExp|^{mconcat $ intersperse " " condList}|]

renderJoin :: (Table, [SqlExpression]) -> SqlBuilder
renderJoin (table, conds) =
  let
    ta = table.getTableAlias
  in
  [PG.sqlExp|JOIN (^{renderFromTable table}) AS ^{ta} ON ^{fromJust $ renderConds conds}|]

renderSelectedColumns :: [SelectedColumn] -> SqlBuilder
renderSelectedColumns columns =
  columns
    & fmap
      ( \case
          SelectFromTable tableName ->
            [PG.sqlExp|^{tableName}.*|]
          SelectNewColumn qe name ->
            [PG.sqlExp|^{renderExpr qe} AS ^{name}|]
          SelectColumnFromTable qualifiedIdentifier ->
            [PG.sqlExp|^{qualifiedIdentifier}|]
      )
    & mconcat . intersperse ", "

renderNameForTableColumn :: Identifier -> Identifier -> SqlBuilder
renderNameForTableColumn tableName columnName =
  [PG.sqlExp|^{compileNameForTableColumn tableName columnName}|]

renderWheres :: [SqlExpression] -> Maybe SqlBuilder
renderWheres [] = Nothing
renderWheres wheres =
  Just [PG.sqlExp|WHERE ^{fromJust $ renderConds wheres}|]

renderConds :: [SqlExpression] -> Maybe SqlBuilder
renderConds [] = Nothing
renderConds conds =
  let condList = fmap renderExpr conds
   in Just [PG.sqlExp|^{mconcat $ intersperse " AND " condList}|]

renderExpr :: SqlExpression -> SqlBuilder
renderExpr (LiteralInt n) =
  [PG.sqlExp|#{n}|]
renderExpr (LiteralFloat n) =
  [PG.sqlExp|#{n}|]
renderExpr (Ref name) =
  [PG.sqlExp|^{name}|]
renderExpr (LiteralString s) =
  [PG.sqlExp|#{s}|]
renderExpr (LiteralBoolean b) =
  [PG.sqlExp|#{b}|]
renderExpr LiteralNull =
  [PG.sqlExp|NULL|]
renderExpr (Call fNotation functionName params) =
  case (fNotation, params) of
    (InfixNotation, [p1, p2]) ->
      infixApply functionName p1 p2
    (DefaultNotation, _) ->
      let renderedParams =
            params
              & fmap (\p -> [PG.sqlExp|^{renderExpr p}|])
              & intersperse ", "
              & mconcat
       in [PG.sqlExp|^{text2SqlBuilder functionName}(^{renderedParams})|]
    (_, _) ->
      error [iii|Call #{fNotation} #{functionName} #{params}|]
  where
    infixApply op param1 param2 =
      [PG.sqlExp|(^{renderExpr param1} ^{text2SqlBuilder op} ^{renderExpr param2})|]
renderExpr (RawSql cosos) =
  cosos
    & fmap
      ( \case
          Left t -> text2SqlBuilder t
          Right e -> renderExpr e
      )
    & mconcat
    & \l -> [PG.sqlExp|(^{l})|]

text2SqlBuilder :: Text -> SqlBuilder
text2SqlBuilder =
  fromString @SqlBuilder . unpack

renderSqlBuilder :: PG.Connection -> SqlBuilder -> IO PG.Query
renderSqlBuilder conn sqlBuilder = do
  (sqlQuery, _logs) <- PG.runSqlBuilder conn PG.defaultLogMasker sqlBuilder
  pure sqlQuery

runQueryBuilder :: (PG.FromRow r) => PG.Connection -> SqlBuilder -> IO [r]
runQueryBuilder conn sqlBuilder = do
  rawQuery <- renderSqlBuilder conn sqlBuilder
  PG.query_ conn rawQuery

-- TODO esto esta repetido en SqlGeneration.hs
qualifyIdentifier :: Identifier -> Identifier -> QualifiedIdentifier
qualifyIdentifier table column =
  QualifiedIdentifier (Just $ fromIdentifier table) (fromIdentifier column)
