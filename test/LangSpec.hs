{-# LANGUAGE GADTs #-}
module LangSpec (spec) where

import Data.Function ((&))
import Data.String.Interpolate
import Data.Text (Text)
import Data.Void (Void)
import qualified Database.PostgreSQL.Simple.Types as PG
import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import qualified LuQL.SqlGeneration
import Test.Syd
import Text.Megaparsec (ParseErrorBundle)
import qualified Database.PostgreSQL.Simple as PG
import LuQL.Runner (SqlRuntimeRow(..))
import Tests.Utils (models)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist)
import GHC.Stack (HasCallStack)

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

