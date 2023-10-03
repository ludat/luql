{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module LuQL.Runner where

import Control.Monad (replicateM)

import Data.Aeson (ToJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (CalendarDiffTime, LocalTime, ZonedTime, scaleCalendarDiffTime)
import Data.Time.Calendar (Day)

import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.FromRow qualified as PG

import GHC.Generics (Generic)

newtype SqlRuntimeRow
  = SqlRuntimeRow (Map Text SqlRuntimeValue)
  deriving (Show)
  deriving newtype (ToJSON)

data SqlRuntimeValue
  = SqlText Text
  | SqlInt Int
  | SqlFloat Scientific
  | SqlBool Bool
  | SqlNull
  | SqlInterval CalendarDiffTime
  | SqlDate Day
  | SqlTimestamp LocalTime
  | SqlTimestampWithTimezone ZonedTime
  | SqlUnknown Text Text
  deriving (Show, Generic, ToJSON)

parseValue :: PG.RowParser (Text, SqlRuntimeValue)
parseValue = do
  PG.fieldWith $ \field maybeContenido -> do
    typename <- PG.typename field
    let name = fromMaybe "" $ PG.name field
    value <- case (typename, maybeContenido) of
      (_, Nothing) -> pure SqlNull
      ("varchar", Just _) -> SqlText <$> PG.fromField field maybeContenido
      ("text", Just _) -> SqlText <$> PG.fromField field maybeContenido
      ("bpchar", Just _) -> SqlText . T.strip <$> PG.fromField field maybeContenido

      ("bool", Just _) -> SqlBool <$> PG.fromField field maybeContenido

      ("int2", Just _) -> SqlInt <$> PG.fromField field maybeContenido
      ("int4", Just _) -> SqlInt <$> PG.fromField field maybeContenido
      ("int8", Just _) -> SqlInt <$> PG.fromField field maybeContenido
      ("float4", Just _) -> SqlFloat <$> PG.fromField field maybeContenido
      ("float8", Just _) -> SqlFloat <$> PG.fromField field maybeContenido
      ("numeric", Just _) -> SqlFloat <$> PG.fromField field maybeContenido

      ("interval", Just v) -> case BS.take 2 v of
        "P-" -> SqlInterval <$> scaleCalendarDiffTime (-1) <$> PG.fromField field (Just $ BS.filter (/= 45) $ v)
        _ -> SqlInterval <$> PG.fromField field maybeContenido

      ("date", Just _) -> SqlDate <$> PG.fromField field maybeContenido
      ("timestamp", Just _) -> SqlTimestamp <$> PG.fromField field maybeContenido
      ("timestamptz", Just _) -> SqlTimestampWithTimezone <$> PG.fromField field maybeContenido
      (sqlType, Just v) -> pure $ SqlUnknown (decodeUtf8 sqlType) (decodeUtf8 v)
    pure (decodeUtf8 name, value)

instance PG.FromRow SqlRuntimeRow where
  fromRow = do
    n <- PG.numFieldsRemaining
    fields <- replicateM n parseValue
    pure $ SqlRuntimeRow $ Map.fromList fields
