module LangSpec
    ( spec
    ) where

import Control.Exception (ErrorCall, bracket, catch, evaluate)
import Control.Monad (when)

import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types qualified as PG

import LuQL.Compiler
import LuQL.Parser
import LuQL.Render
import LuQL.Runner (SqlRuntimeRow (..))
import LuQL.SqlGeneration qualified

import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (splitExtensions)
import System.Timeout

import Test.Hspec

import Tests.Utils (models)

import Text.Megaparsec (errorBundlePretty)

spec :: Spec
spec =  aroundAll withDatabase $ do
  itMatchesSnapshot "from model"
    [__i|
      from Languages
    |]

  itMatchesSnapshot "query with invalid statement"
    [__i|
      something invalid
    |]

  itMatchesSnapshot "query with a newline"
    [__i|
      from Languages

      let a = 7
    |]

  itMatchesSnapshot "query with a newline before beginning of the query"
    [i|


from Languages
let a = 7
|]

  itMatchesSnapshot "query with a comment before beginning of the query"
    [i|

-- comments
from Languages
let a = 7
|]

  itMatchesSnapshot "query with a comment after the end of the query"
    [i|
from Languages
let a = 7
-- comments

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

  itMatchesSnapshot "a return keeps a simple column"
    [__i|
        let a = 7
        select a
    |]

  itMatchesSnapshot "a return keeps a model column"
    [__i|
        from Languages as l
        select l.language_id
    |]

  itMatchesSnapshot "query with a barChart"
    [__i|
      from Languages as l
      barChart l.language_id, l.language_id
    |]

withDatabase :: (PG.Connection -> IO ()) -> IO ()
withDatabase action = bracket
  (PG.connectPostgreSQL "postgres://postgres:123456@localhost:5432/postgres")
  PG.close
  action

shouldMatchSnapshot :: (HasCallStack, ToJSON a) => a -> String -> IO ()
shouldMatchSnapshot actualToShow filepath = do
  let actual = actualToShow & encodeToJSON
  shouldMatchSnapshot' [i|#{actual}|] filepath

addExtension :: String -> FilePath -> FilePath
addExtension newExtension filepath =
  let
    (filepathWithoutExtensions, originalExtensions) = splitExtensions filepath
    newExtensions = newExtension <> originalExtensions
  in filepathWithoutExtensions <> newExtensions

shouldMatchSnapshot' :: HasCallStack => ByteString -> FilePath -> IO ()
shouldMatchSnapshot' actual filepath = do
  let expectedFilepath = addExtension ".expected" filepath
  let actualFilepath = addExtension ".actual" filepath

  expectedFilepathExists <- doesFileExist expectedFilepath
  if expectedFilepathExists
    then do
      expectedText <- BS.readFile expectedFilepath

      if (actual /= expectedText)
        then do
          BS.writeFile actualFilepath actual
          fail [i|expected and actual are not equal: diff #{expectedFilepath} #{actualFilepath}"|]
        else do
          removeFileIfExists actualFilepath

    else do
      BS.writeFile actualFilepath actual
      fail [__i|expected result file not present: #{expectedFilepath}, actual result is written to #{actualFilepath}|]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists filepath = do
  exists <- doesFileExist filepath
  when exists $ do
    removeFile filepath

itMatchesSnapshot :: HasCallStack => String -> Text -> SpecWith PG.Connection
itMatchesSnapshot name program = describe name $ do
  it "matches the snapshots" $ \conn -> ((do
    createDirectoryIfMissing True ("test/.golden/Lang/" <> name)
    Text.encodeUtf8 program `shouldMatchSnapshot'` [i|test/.golden/Lang/#{name}/0_Raw.luql|]

    parsedProgram :: RawQuery
      <- timeout 100_000 (evaluate (parseQuery program))
        & fmap (fromMaybe (error "timeout parsing"))
        & fmap (either (error . errorBundlePretty) id)

    parsedProgram `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/1_Parser.json|]

    let compiledProgram :: Either [Error] CompiledQuery
        compiledProgram =
          parsedProgram
            & compileProgram models

    compiledProgram `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/2_Compiler.json|]

    let partialSqlQuery :: LuQL.SqlGeneration.PartialQuery
        partialSqlQuery =
          compiledProgram
            & either (\e -> error [i|#{e}|]) id
            & (.compiledStatements)
            & LuQL.SqlGeneration.compileStatements

    partialSqlQuery `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/3_SqlGeneration.json|]

    rawSqlQuery <-
      partialSqlQuery
        & renderPartialQuery
        & renderSqlBuilder conn

    PG.fromQuery rawSqlQuery `shouldMatchSnapshot'` [i|test/.golden/Lang/#{name}/4_Render.sql|]

    queryResults <- PG.query_ @SqlRuntimeRow conn rawSqlQuery
    queryResults `shouldMatchSnapshot` [i|test/.golden/Lang/#{name}/5_Run.json|]
    ) `catch` (\(e :: ErrorCall) ->
      [i|#{show e}|] `shouldMatchSnapshot'` [i|test/.golden/Lang/#{name}/999_Exception.txt|]
    ))

encodeToJSON :: ToJSON a => a -> ByteString
encodeToJSON = BS.toStrict . encodePretty'
  defConfig
  { confIndent = Spaces 2
  , confTrailingNewline = True
  }
