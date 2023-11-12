{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module LuQL.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Data.Char (isAlphaNum)
import Data.Function ((&))
import Data.Text (Text)
import Data.Void (Void)

import GHC.Generics (Generic)

import LuQL.Types

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype RawQuery = RawQuery {unRawQuery :: [QueryStatement Raw]}
  deriving (Eq, Generic, Show)

data Raw

type instance StmtE _ "ctx" Raw = Range

type instance StmtE "from" "model" Raw = (Maybe Text, QueryExpression Raw)

type instance StmtE "join" "model" Raw = (Maybe Text, QueryExpression Raw)

type instance StmtE "join" "on" Raw = Maybe (QueryExpression Raw)

type instance StmtE "ext" "ext" Raw = ExtStmtRaw

data ExtStmtRaw
  = ExtStmtInvalid (StmtE "invalid" "ctx" Raw) Text
  | ExtStmtEmptyLine (StmtE "invalid" "ctx" Raw)
  deriving (Show, Generic, Eq)

type instance ExprE _ "ctx" Raw = Range

type instance ExprE "apply" "function" Raw = (QueryExpression Raw)

type instance ExprE "ext" "ext" Raw = ExtExprRaw

data ExtExprRaw
  = ExtExprEmptyExpr (ExprE "empty" "ctx" Raw)
  deriving (Show, Generic, Eq)

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
  (p, identifier) <- withLocation $ do
    _ <- symbol "from"
    expressionParser
  modelName <- optional $ do
    _ <- symbol "as"
    identifierParser

  pure $ From p (modelName, identifier)

type Position = Int

data Range = Range
  { begin :: Position
  , end :: Position
  } deriving (Eq, Generic, Show)

nullRange :: Range
nullRange = Range -1 -1

type Located a = (Range, a)

withLocation :: Parser a -> Parser (Located a)
withLocation p = do
  initialPosition <- getOffset
  result <- p
  finalPosition <- getOffset
  pure (Range initialPosition finalPosition, result)

expressionParser :: Parser (QueryExpression Raw)
expressionParser = do
  makeExprParser (simpleExpressionParser) [
      [ prefix "!"
      ],
      [ infixL "??"
      ],
      [ infixL "<="
      , infixL ">="
      , infixL "<"
      , infixL ">"
      ],
      [ infixL "=="
      , infixL "!="
      ],
      [ infixL "&&"
      , infixL "||"
      ],
      [ infixL "**"
      ],
      [ infixL "*"
      , infixL "/"
      , infixL "%"
      ],
      [ infixL "+"
      , infixL "-"
      ]
    ]
  where
    infixL :: Text -> Operator Parser (QueryExpression Raw)
    infixL op = InfixL $ do
      (pos, _) <- withLocation $ symbol op
      pure (\expr1 expr2 ->
        Apply
          (Range (expr1 & getSrcRange & (.begin)) (expr2 & getSrcRange & (.end)))
          (Ref pos op)
          [expr1, expr2]
        )

    prefix :: Text -> Operator Parser (QueryExpression Raw)
    prefix op = Prefix $ do
      (pos, _) <- withLocation $ symbol op
      pure (\expr ->
        Apply
          (expr & getSrcRange)
          (Ref pos op)
          [expr]
        )

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
      <|> emptyParser

  postfixOperations <- many (eitherP (withLocation applyParser) (withLocation propParser))
  pure $
    foldl
      ( \e postfixOperation ->
          case postfixOperation of
            Left (pos, params) -> Apply pos e params
            Right (pos, propName) -> Prop pos e propName
      )
      expr
      postfixOperations

emptyParser :: Parser (QueryExpression Raw)
emptyParser = do
  pos <- getOffset
  pure (ExprExt $ ExtExprEmptyExpr $ Range pos pos)

ifParser :: Parser (QueryExpression Raw)
ifParser = do
  (pos, _) <- withLocation $ symbol "if"
  condExpr <- expressionParser
  _ <- withLocation $ symbol "then"
  thenExpr <- expressionParser
  _ <- withLocation $ symbol "else"
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
  pure $ RawSql (Range startPos endPos) coso

applyParser :: Parser [QueryExpression Raw]
applyParser = do
  between (symbol "(") (symbol ")") (expressionParser `sepBy` symbol ",")

propParser :: Parser Text
propParser = do
  _ <- char '.'
  try identifierParser <|> (pure "" <* spaceConsumer)

stringParser :: Parser (QueryExpression Raw)
stringParser = lexeme $ do
  (pos, text) <- withLocation $ do
    _ <- char '"'
    text <- takeWhileP Nothing (/= '"')
    _ <- char '"'
    pure text
  pure $ Lit pos $ LiteralString text

floatParser :: Parser (QueryExpression Raw)
floatParser = do
  (pos, n) <- lexeme $ withLocation $ L.signed spaceConsumer L.float
  pure $ Lit pos $ LiteralFloat n

boolParser :: Parser (QueryExpression Raw)
boolParser = do
  (pos, bool) <-
    lexeme $
      withLocation $
        (string "true" >> pure True)
          <|> (string "false" >> pure False)
  pure $ Lit pos $ LiteralBoolean bool

numberParser :: Parser (QueryExpression Raw)
numberParser = do
  (pos, n) <- lexeme $ withLocation $ L.signed spaceConsumer L.decimal
  pure $ Lit pos $ LiteralInt n

nullParser :: Parser (QueryExpression Raw)
nullParser = do
  (pos, _) <- withLocation $ symbol "null"
  pure $ Lit pos LiteralNull

refParser :: Parser (QueryExpression Raw)
refParser = do
  (pos, ref) <- withLocation identifierParser
  pure $ Ref pos ref

groupByParser :: Parser (QueryStatement Raw)
groupByParser = do
  (pos, _) <- withLocation $ symbol "group"
  _ <- symbol "by"
  groupByColumns <- asParser `sepBy1` symbol ","
  lets <-
    between
      (L.symbol spaceConsumerWithNewline "{")
      (L.symbol spaceConsumer "}")
      (many (letParser <* spaceConsumer <* eol))
  pure $ GroupBy pos groupByColumns lets

orderDirectionParser :: Parser OrderDirection
orderDirectionParser = do
  (symbol "asc" *> pure Asc) <|> (symbol "desc" *> pure Desc)

orderByParser :: Parser (QueryStatement Raw)
orderByParser = do
  (pos, _) <- withLocation $ symbol "order"
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
  (pos, values) <- withLocation $ do
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
  (pos, expr) <- withLocation $ do
    void $ symbol "where"
    expressionParser
  pure $ Where pos expr

joinParser :: Parser (QueryStatement Raw)
joinParser = do
  (pos, identifier) <- withLocation $ do
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
  (pos, (identifier, expression)) <- withLocation $ do
    _ <- symbol "let"
    identifier <- identifierParser
    _ <- symbol "="
    expression <- expressionParser
    pure (identifier, expression)
  pure $ Let pos identifier expression

statementParser :: Parser (QueryStatement Raw)
statementParser = do
  fromParser
  <|> whereParser
  <|> returnParser
  <|> letParser
  <|> joinParser
  <|> groupByParser
  <|> orderByParser
  <|> emptyLineParser
  <|> invalidStatementParser

emptyLineParser :: Parser (QueryStatement Raw)
emptyLineParser = do
  pos <- getOffset
  void $ optional spaceConsumer

  pure $ StmtExt $ ExtStmtEmptyLine (Range pos pos)

invalidStatementParser :: Parser (QueryStatement Raw)
invalidStatementParser = do
  (pos, text) <- withLocation $ takeWhile1P Nothing (/= '\n')
  pure $ StmtExt $ ExtStmtInvalid pos text

class HasSrcRange a where
  getSrcRange :: a -> Range

instance HasSrcRange (QueryStatement Raw) where
  getSrcRange :: QueryStatement Raw -> Range
  getSrcRange (From pos _) = pos
  getSrcRange (Where pos _) = pos
  getSrcRange (GroupBy pos _ _) = pos
  getSrcRange (Join pos _ _) = pos
  getSrcRange (Let pos _ _) = pos
  getSrcRange (OrderBy pos _) = pos
  getSrcRange (Return pos _) = pos
  getSrcRange (StmtExt (ExtStmtInvalid pos _)) = pos
  getSrcRange (StmtExt (ExtStmtEmptyLine pos)) = pos


instance HasSrcRange (QueryExpression Raw) where
  getSrcRange :: QueryExpression Raw -> Range
  getSrcRange (Lit pos _) = pos
  getSrcRange (Prop pos _ _) = pos
  getSrcRange (Ref pos _) = pos
  getSrcRange (Apply pos _ _) = pos
  getSrcRange (RawSql pos _) = pos
  getSrcRange (If pos _ _ _) = pos
  getSrcRange (ExprExt (ExtExprEmptyExpr pos)) = pos

parser :: Parser RawQuery
parser = do
  RawQuery <$> ((statementParser <* spaceConsumer) `sepBy` eol)

parseQuery :: Text -> Either (ParseErrorBundle Text Void) RawQuery
parseQuery = parse (parser <* eof) ""
