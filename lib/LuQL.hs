module LuQL where

import Data.String.Interpolate (iii)
import Data.Text (Text)

import Database.PostgreSQL.Query
import Database.PostgreSQL.Query qualified as PG

import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import LuQL.Runner
import LuQL.SqlGeneration

completeQuery :: Models -> Int -> RawQuery -> [Completion]
completeQuery models position rawQuery = do
  LuQL.Compiler.generateCompletions (pTraceShowIdForceColor position) models $ pTraceShowIdForceColor rawQuery

compileQuery :: Models -> RawQuery -> SqlBuilder
compileQuery models rawQuery = do
  let typecheckedStatements = case LuQL.Compiler.compileProgram models rawQuery of
        (Right qss) -> qss
        (Left errors) ->
          error [iii|errores aparecieron en la query: #{errors}|]

      partialQuery = LuQL.SqlGeneration.compileStatements typecheckedStatements.unCompiledQuery

  renderPartialQuery partialQuery

runQuery :: Models -> PG.Connection -> RawQuery -> IO [SqlRuntimeRow]
runQuery models conn program = do
  let sqlBuilder = compileQuery models program
  runQueryBuilder conn [PG.sqlExp|^{sqlBuilder}|]
