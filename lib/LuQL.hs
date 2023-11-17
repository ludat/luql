{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module LuQL where

import Data.String.Interpolate (iii)

import Database.PostgreSQL.Query
import Database.PostgreSQL.Query qualified as PG

import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import LuQL.Runner
import LuQL.SqlGeneration
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson

completeQuery :: Models -> Int -> RawQuery -> [Completion]
completeQuery models position rawQuery = do
  LuQL.Compiler.generateCompletions position models rawQuery

compileQuery :: Models -> RawQuery -> SqlBuilder
compileQuery models rawQuery = do
  let typecheckedStatements = case LuQL.Compiler.compileProgram models rawQuery of
        (Right qss) -> qss
        (Left errors) ->
          error [iii|errores aparecieron en la query: #{errors}|]

      partialQuery = LuQL.SqlGeneration.compileStatements typecheckedStatements.compiledStatements

  renderPartialQuery partialQuery

data QueryResult = QueryResult
  { rows :: [SqlRuntimeRow]
  , columns :: [Text]
  , graph :: Maybe DrawStrategy
  } deriving (Show, Generic, ToJSON)

runQuery :: Models -> PG.Connection -> RawQuery -> IO QueryResult
runQuery models conn program = do
  let compiledQuery = case LuQL.Compiler.compileProgram models program of
        (Right result) ->
          result
        (Left errors) ->
          error [iii|errores aparecieron en la query: #{errors}|]

  let partialQuery = LuQL.SqlGeneration.compileStatements compiledQuery.compiledStatements

  let sqlBuilder = renderPartialQuery partialQuery
  rows <- runQueryBuilder conn sqlBuilder
  pure $
    QueryResult
    rows
    compiledQuery.resultColumns
    compiledQuery.graph
