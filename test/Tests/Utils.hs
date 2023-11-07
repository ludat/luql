module Tests.Utils where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

import LuQL.Compiler
import LuQL.Parser (nullRange)
import LuQL.Types


models :: Map Text ModelDefinition
models =
  Map.fromList
    [ ("Countries", countries)
    , ("Cities", cities)
    , ("Customers", customers)
    , ("Languages", languages)
    , ("Films", films)
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
              ]
        }
    films =
      ModelDefinition
        { tableName = "film",
          defaultSingularName = "film",
          columns =
            Map.fromList
              [ ("film_id", IntType)
              , ("title", StringType)
              , ("description", StringType)
              , ("language_id", IntType)
              ],
          implicitWhere = Nothing,
          relatedTables =
            Map.fromList
              [ ("language", ("language_id", "language_id"))
              ]
        }
    italian =
      languages
        { defaultSingularName = "italian"
        , implicitWhere = Just $ \model ->
            Apply
              nullRange
              (Ref nullRange "==")
              [ Prop nullRange (Ref nullRange model) "name"
              , Lit nullRange $ LiteralString "Italian"
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
              nullRange
              (Ref nullRange "==")
              [ Prop nullRange (Ref nullRange model) "active"
              , Lit nullRange $ LiteralInt 1
              ],
          relatedTables =
            Map.fromList
              [
              ]
        }
