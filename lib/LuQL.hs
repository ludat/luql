module LuQL where

import Data.String.Interpolate (iii)

import Database.PostgreSQL.Query
import Database.PostgreSQL.Query qualified as PG

import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import LuQL.Runner
import LuQL.SqlGeneration

compileQuery :: Models -> RawQuery -> SqlBuilder
compileQuery models (RawQuery queryStatements) = do
  let typecheckedStatements = case LuQL.Compiler.compileStatements models queryStatements of
        (qss, TypeInfo {errors = []}) -> qss
        (_, ti) ->
          let
            errors = ti.errors
          in error [iii|errores aparecieron en la query: #{errors}|]

      partialQuery = LuQL.SqlGeneration.compileStatements typecheckedStatements

  renderPartialQuery partialQuery

runQuery :: Models -> PG.Connection -> RawQuery -> IO [SqlRuntimeRow]
runQuery models conn program = do
  let sqlBuilder = compileQuery models program
  runQueryBuilder conn [PG.sqlExp|^{sqlBuilder}|]
