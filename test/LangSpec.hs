{-# LANGUAGE GADTs #-}
module LangSpec
    ( spec
    ) where

import Data.Function ((&))
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)

import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types qualified as PG

import GHC.Stack (HasCallStack)

import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import LuQL.Runner (SqlRuntimeRow (..))
import LuQL.SqlGeneration qualified

import System.Directory (createDirectoryIfMissing, doesFileExist)

import Test.Syd

import Tests.Utils (models)

import Text.Megaparsec (ParseErrorBundle)

spec :: Spec
spec =  aroundAll withDatabase $ doNotRandomiseExecutionOrder $ do
  itMatchesSnapshot "from model"
    [__i|
      from Languages
    |]

  itMatchesSnapshot "from model assigning a different name"
    [__i|
      from Languages as l
    |]

  itMatchesSnapshot "from model with implicit wheres"
    [__i|
      from Customers
    |]

  itMatchesSnapshot "where with a true condition"
    [__i|
      from Languages
      where true
    |]

  itMatchesSnapshot "join with a valid model"
    [__i|
      from Countries
      join Cities
    |]

  itMatchesSnapshot "let a new column with a constant"
    [__i|
      from Languages
      let column = 0
    |]

  itMatchesSnapshot "group by with a raw query (count)"
    [__i|
      from Cities
      group by city.country_id {
        let count = `count(*)`
      }
    |]

  itMatchesSnapshot "group by with an aggregate"
    [__i|
      from Cities
      group by city.country_id {
        let max_city = max(city.country_id)
      }
    |]

  itMatchesSnapshot "group by with a declared column"
    [__i|
      let kept = 7
      let lost = 2
      group by kept {
        let new = kept
      }
      order by new
      order by kept
    |]

  itMatchesSnapshot "a single let with a number"
    [__i|
      let a = 23
    |]

  itMatchesSnapshot "a single let with a from afterwards"
    [__i|
      let a = 23
      from Languages
    |]
  itMatchesSnapshot "a single join with a rename"
    [__i|
        from Languages as l
        join Films
    |]
  itMatchesSnapshot "a return keeps only that column"
    [__i|
        from Languages as l
        return l.language_id
    |]

withDatabase :: (PG.Connection -> IO ()) -> IO ()
withDatabase action = do
  conn <- PG.connectPostgreSQL "postgres://postgres:123456@localhost:5432/postgres"
  _ <- action conn
  PG.close conn

shouldMatchSnapshot :: (HasCallStack, Show a) => a -> String -> IO ()
shouldMatchSnapshot actualToShow filepath = do
  let actual = actualToShow & ppShow & Text.pack
  shouldMatchSnapshot' actual filepath


shouldMatchSnapshot' :: HasCallStack => Text -> String -> IO ()
shouldMatchSnapshot' actual filepath = do
  targetPathExists <- doesFileExist filepath
  if not targetPathExists
    then Text.writeFile filepath actual
    else do
      expectedText <- Text.readFile filepath
      Text.writeFile filepath actual
      actual `shouldBe` expectedText

itMatchesSnapshot :: HasCallStack => String -> Text -> TestDefM '[PG.Connection] () ()
itMatchesSnapshot name program = describe name $ do
  itWithOuter "matches the snapshots" $ \conn -> do
    createDirectoryIfMissing True ("test/.golden/Lang/" <> name)
    context "original program" $
      program `shouldMatchSnapshot'` [i|test/.golden/Lang/#{name}/0_Raw.golden|]

    let parsedProgram :: Either (ParseErrorBundle Text Void) RawQuery
        parsedProgram =
          parseQuery program

    context "parsed program" $
      parsedProgram `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/1_Parser.golden|]

    let compiledProgram :: Either [Error] CompiledQuery
        compiledProgram =
          parsedProgram
            & either (error . show) id
            & compileProgram models

    context "compiled program" $
      compiledProgram `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/2_Compiler.golden|]

    let partialSqlQuery :: LuQL.SqlGeneration.PartialQuery
        partialSqlQuery =
          compiledProgram
            & either (error . ppShow) id
            & (.unCompiledQuery)
            & LuQL.SqlGeneration.compileStatements

    context "sql query in haskell" $
      partialSqlQuery `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/3_SqlGeneration.golden|]

    rawSqlQuery <-
      partialSqlQuery
        & renderPartialQuery
        & renderSqlBuilder conn

    context "raw sql query" $
      (Text.decodeUtf8 $ PG.fromQuery rawSqlQuery) `shouldMatchSnapshot'` [i|test/.golden/Lang/#{name}/4_Render.golden|]

    queryResults <- PG.query_ @SqlRuntimeRow conn rawSqlQuery
    context "query results" $
      queryResults `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/5_Run.golden|]

