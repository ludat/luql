{-# LANGUAGE GADTs #-}
module LangSpec where

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
import Data.Map (Map)
import Data.Map qualified as Map
import LuQL.Types (QueryExpression(..), LiteralValue(..))
import LuQL.Runner (SqlRuntimeRow(..))

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

withDatabase :: (PG.Connection -> IO ()) -> IO ()
withDatabase action = do
  conn <- PG.connectPostgreSQL "postgres://postgres:123456@localhost:5432/postgres"
  _ <- action conn
  PG.close conn

itMatchesSnapshot :: String -> Text -> TestDefM '[PG.Connection]  () ()
itMatchesSnapshot name program = describe name $ do
  let parsedProgram :: Either (ParseErrorBundle Text Void) RawQuery
      parsedProgram =
        parseQuery program

  it "0_Raw" $
    pureGoldenTextFile [i|test/.golden/Lang/#{name}/0_Raw.golden|] $
      program

  it "1_Parser" $
    goldenPrettyShowInstance [i|test/.golden/Lang/#{name}/1_Parser.golden|] $
      parsedProgram

  let compiledProgram :: Either [Error] CompiledQuery
      compiledProgram =
        parsedProgram
          & either (error . show) id
          & compileProgram models
  it "2_Compiler" $
    goldenPrettyShowInstance [i|test/.golden/Lang/#{name}/2_Compiler.golden|] $
      compiledProgram

  let partialSqlQuery :: LuQL.SqlGeneration.PartialQuery
      partialSqlQuery =
        compiledProgram
          & either (error . show) id
          & unCompiledQuery
          & LuQL.SqlGeneration.compileStatements
  it "3_SqlGeneration" $
    goldenPrettyShowInstance [i|test/.golden/Lang/#{name}/3_SqlGeneration.golden|] $
      partialSqlQuery

  beforeAllWith (\conn -> do
      rawSqlQuery <-
        partialSqlQuery
          & renderPartialQuery
          & renderSqlBuilder conn
      pure rawSqlQuery
    ) $ do
    itWithOuter "4_Render" $ \(rawSqlQuery :: PG.Query) -> do
      pureGoldenByteStringFile [i|test/.golden/Lang/#{name}/4_Render.golden|] $
        PG.fromQuery rawSqlQuery

    itWithAll "5_Run" (\(HCons rawSqlQuery (HCons conn HNil) :: HList '[PG.Query, PG.Connection]) () -> do
      queryResults <- PG.query_ @SqlRuntimeRow conn rawSqlQuery
      pure $ goldenPrettyShowInstance [i|test/.golden/Lang/#{name}/5_Run.golden|] $
        queryResults
      )

models :: Map Text ModelDefinition
models =
  Map.fromList
    [ ("Countries", countries)
    , ("Cities", cities)
    , ("Customers", customers)
    , ("Languages", languages)
    , ("Italian", italian)
    ]
  where
    countries =
      ModelDefinition
        { tableName = "country",
          defaultSingularName = "country",
          columns =
            Map.fromList
              [ ("country_id", IntType)
              , ("country", StringType)
              , ("last_update", TimestampType)
              ],
          implicitWhere = Nothing,
          relatedTables =
            Map.fromList
              [ ("city", ("country_id", "country_id"))
              ]
        }
    languages =
      ModelDefinition
        { tableName = "language",
          defaultSingularName = "language",
          columns =
            Map.fromList
              [ ("language_id", IntType)
              , ("name", StringType)
              , ("last_update", TimestampType)
              ],
          implicitWhere = Nothing,
          relatedTables =
            Map.fromList
              [ ("film", ("language_id", "language_id"))
              , ("film", ("language_id", "original_language_id"))
              ]
        }
    italian =
      languages
        { defaultSingularName = "italian"
        , implicitWhere = Just $ \model ->
            Apply
              (0, 0)
              (Ref (0, 0) "==")
              [ Prop (0, 0) (Ref (0, 0) model) "name"
              , Lit (0, 0) $ LiteralString "Italian"
              ]
        }
    cities =
      ModelDefinition
        { tableName = "city",
          defaultSingularName = "city",
          columns =
            Map.fromList
              [ ("city_id", IntType)
              , ("city", StringType)
              , ("country_id", IntType)
              , ("last_update", TimestampType)
              ],
          implicitWhere = Nothing,
          relatedTables =
            Map.fromList
              [ ("country", ("country_id", "country_id"))
              ]
        }
    customers =
      ModelDefinition
        { tableName = "customer",
          defaultSingularName = "customer",
          columns =
            Map.fromList
              [ ("customer_id", IntType)
              , ("store_id", IntType)
              , ("first_name", StringType)
              , ("last_name", StringType)
              , ("email", StringType)
              , ("address_id", StringType)
              , ("activebool", BooleanType)
              , ("create_date", DateType)
              , ("last_update", TimestampType)
              , ("active", IntType)
              ],
          implicitWhere = Just $ \model ->
            Apply
              (0, 0)
              (Ref (0, 0) "==")
              [ Prop (0, 0) (Ref (0, 0) model) "active"
              , Lit (0, 0) $ LiteralInt 1
              ],
          relatedTables =
            Map.fromList
              [
              ]
        }
