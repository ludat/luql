{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module LuQL.Parser where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import LuQL.Types
import Safe (headMay, lastMay)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

newtype RawQuery = RawQuery {unRawQuery :: [QueryStatement Raw]}
  deriving (Eq, Generic, Show)

data Raw

type instance StmtE _ "ctx" Raw = (Int, Int)

type instance StmtE "from" "model" Raw = (Maybe Text, QueryExpression Raw)

type instance StmtE "join" "model" Raw = (Maybe Text, QueryExpression Raw)

type instance StmtE "join" "on" Raw = Maybe (QueryExpression Raw)

type instance StmtE "ext" "ext" Raw = Void

type instance ExprE _ "ctx" Raw = (Int, Int)

type instance ExprE "apply" "function" Raw = (QueryExpression Raw)

type instance ExprE "ext" "ext" Raw = Void

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    hspace1
    (L.skipLineComment "--")
    (L.skipBlockComment "/*" "*/")

spaceConsumerWithNewline :: Parser ()
spaceConsumerWithNewline =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol' spaceConsumer

identifierParser :: Parser ModelName
identifierParser = lexeme $ takeWhile1P Nothing (\c -> isAlphaNum c || c == '_')

fromParser :: Parser (QueryStatement Raw)
fromParser = do
  (p, identifier) <- withPosition $ do
    _ <- symbol "from"
    expressionParser
  modelName <- optional $ do
    _ <- symbol "as"
    identifierParser

  pure $ From p (modelName, identifier)

operators :: [Text]
operators =
  ["<=", "<", ">=", ">", "==", "!=", "&&", "||", "*", "/", "%", "+", "-", "??"]

type WithPosition a = ((Int, Int), a)

withPosition :: Parser a -> Parser (WithPosition a)
withPosition p = do
  initialPosition <- getOffset
  result <- p
  finalPosition <- getOffset
  pure ((initialPosition, finalPosition), result)

-- TODO esta locura se puede reemplazar con
-- Control.Monad.Combinators.makeExprParser del paquete parser-combinators
expressionParser :: Parser (QueryExpression Raw)
expressionParser = do
  firstExpr <- withPosition simpleExpressionParser
  opsExprs <-
    concat
      <$> many
        ( do
            op <- withPosition $ choice $ fmap symbol operators
            expr <- withPosition simpleExpressionParser
            pure [Left op, Right expr]
        )
  pure $ replaceOpWithApply operators $ Right firstExpr : opsExprs

replaceOpWithApply ::
  [Text] ->
  [Either (WithPosition Text) (WithPosition (QueryExpression Raw))] ->
  QueryExpression Raw
replaceOpWithApply _ [Right (_pos, expr)] = expr
replaceOpWithApply ops@(op : restOps) cosos =
  let firstPos =
        headMay cosos
          & ( \case
                Nothing -> undefined
                Just (Left ((pos, _), _)) -> pos
                Just (Right ((pos, _), _)) -> pos
            )
      lastPos =
        lastMay cosos
          & ( \case
                Nothing -> undefined
                Just (Left ((_, pos), _)) -> pos
                Just (Right ((_, pos), _)) -> pos
            )
   in case splitOn ((== Left op) . mapLeft snd) cosos of
        Just (left, Left (posO, _op), right) -> Apply (firstPos, lastPos) (Ref posO op) [replaceOpWithApply restOps left, replaceOpWithApply ops right]
        Just (_left, Right (_posO, _expr), _right) -> error "imposible" -- porque estoy buscando un left arriba, deberia cambiar la funciona de abajo para mapear"
        Nothing -> replaceOpWithApply restOps cosos
replaceOpWithApply _ _ = error "the impossible happened"

splitOn :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitOn f xs =
  case break f xs of
    (_, []) -> Nothing
    (beforeList, element : afterList) -> Just (beforeList, element, afterList)

simpleExpressionParser :: Parser (QueryExpression Raw)
simpleExpressionParser = do
  expr <-
    between (symbol "(") (symbol ")") expressionParser
      <|> ifParser
      <|> rawSqlParser
      <|> boolParser
      <|> stringParser
      <|> try floatParser
      <|> try numberParser
      <|> nullParser
      <|> refParser

  postfixOperations <- many (eitherP (withPosition applyParser) (withPosition propParser))
  pure $
    foldl
      ( \e postfixOperation ->
          case postfixOperation of
            Left (pos, params) -> Apply pos e params
            Right (pos, propName) -> Prop pos e propName
      )
      expr
      postfixOperations

ifParser :: Parser (QueryExpression Raw)
ifParser = do
  (pos, _) <- withPosition $ symbol "if"
  condExpr <- expressionParser
  _ <- withPosition $ symbol "then"
  thenExpr <- expressionParser
  _ <- withPosition $ symbol "else"
  elseExpr <- expressionParser
  pure $ If pos condExpr thenExpr elseExpr

rawSqlParser :: Parser (QueryExpression Raw)
rawSqlParser = do
  startPos <- getOffset
  _ <- char '`'
  coso <-
    many $
      eitherP
        (takeWhile1P Nothing (\c -> c /= '$' && c /= '`'))
        ( do
            _ <- char '$'
            between (symbol "{") (char '}') expressionParser
        )
  _ <- char '`'
  endPos <- getOffset
  spaceConsumer
  pure $ RawSql (startPos, endPos) coso

applyParser :: Parser [QueryExpression Raw]
applyParser = do
  try $ between (symbol "(") (symbol ")") (expressionParser `sepBy` symbol ",")

propParser :: Parser Text
propParser =
  try $ do
    _ <- char '.'
    identifierParser

stringParser :: Parser (QueryExpression Raw)
stringParser = lexeme $ do
  (pos, text) <- withPosition $ do
    _ <- char '"'
    text <- takeWhileP Nothing (/= '"')
    _ <- char '"'
    pure text
  pure $ Lit pos $ LiteralString text

floatParser :: Parser (QueryExpression Raw)
floatParser = do
  (pos, n) <- lexeme $ withPosition $ L.signed spaceConsumer L.float
  pure $ Lit pos $ LiteralFloat n

boolParser :: Parser (QueryExpression Raw)
boolParser = do
  (pos, bool) <-
    lexeme $
      withPosition $
        (string "true" >> pure True)
          <|> (string "false" >> pure False)
  pure $ Lit pos $ LiteralBoolean bool

numberParser :: Parser (QueryExpression Raw)
numberParser = do
  (pos, n) <- lexeme $ withPosition $ L.signed spaceConsumer L.decimal
  pure $ Lit pos $ LiteralInt n

nullParser :: Parser (QueryExpression Raw)
nullParser = do
  (pos, _) <- withPosition $ symbol "null"
  pure $ Lit pos LiteralNull

refParser :: Parser (QueryExpression Raw)
refParser = do
  (pos, ref) <- withPosition identifierParser
  pure $ Ref pos ref

groupByParser :: Parser (QueryStatement Raw)
groupByParser = do
  (pos, _) <- withPosition $ symbol "group"
  _ <- symbol "by"
  groupByColumns <- asParser `sepBy1` symbol ","
  lets <-
    between
      (L.symbol spaceConsumerWithNewline "{")
      (L.symbol spaceConsumerWithNewline "}")
      (many (letParser <* spaceConsumerWithNewline))
  pure $ GroupBy pos groupByColumns lets

orderDirectionParser :: Parser OrderDirection
orderDirectionParser = do
  (symbol "asc" *> pure Asc) <|> (symbol "desc" *> pure Desc)

orderByParser :: Parser (QueryStatement Raw)
orderByParser = do
  (pos, _) <- withPosition $ symbol "order"
  _ <- symbol "by"
  orderExpr <-
    ( do
        expr <- expressionParser
        orderDirection <- optional orderDirectionParser
        pure (expr, orderDirection)
      )
      `sepBy1` symbol ","
  pure $ OrderBy pos orderExpr

returnParser :: Parser (QueryStatement Raw)
returnParser = do
  (pos, values) <- withPosition $ do
    _ <- symbol "return"
    expressionParser `sepBy1` symbol ","
  pure $ Return pos values

asParser :: Parser (QueryExpression Raw, Maybe Text)
asParser = do
  expr <- expressionParser
  coso <- withRecovery (const $ pure Nothing) $ do
    _ <- symbol "as"
    Just <$> identifierParser
  pure (expr, coso)

whereParser :: Parser (QueryStatement Raw)
whereParser = do
  (pos, expr) <- withPosition $ do
    void $ symbol "where"
    expressionParser
  pure $ Where pos expr

joinParser :: Parser (QueryStatement Raw)
joinParser = do
  (pos, identifier) <- withPosition $ do
    _ <- symbol "join"
    expressionParser
  modelName <- optional $ do
    _ <- symbol "as"
    identifierParser
  onExpr <- optional $ do
    _ <- symbol "on"
    expressionParser
  pure $ Join pos (modelName, identifier) onExpr

letParser :: Parser (QueryStatement Raw)
letParser = do
  (pos, (identifier, expression)) <- withPosition $ do
    _ <- symbol "let"
    identifier <- identifierParser
    _ <- symbol "="
    expression <- expressionParser
    pure (identifier, expression)
  pure $ Let pos identifier expression

statementParser :: Parser (QueryStatement Raw)
statementParser = do
  fromParser <|> whereParser <|> returnParser <|> letParser <|> joinParser <|> groupByParser <|> orderByParser

parser :: Parser RawQuery
parser = do
  spaceConsumerWithNewline
  RawQuery <$> many (statementParser <* spaceConsumerWithNewline)

parseQuery :: Text -> Either (ParseErrorBundle Text Void) RawQuery
parseQuery = parse (parser <* eof) ""
