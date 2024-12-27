module Hasql.Generator.Internal.Database.Sql.Analysis2
  ( getColumnMetadata,
  )
where

import Control.Applicative (pure, (<*>))
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Function (($))
import Data.Functor (fmap, (<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (toList)
import Hasql.Decoders (Result, column, rowVector)
import Hasql.Decoders qualified as Decoders (bool, nonNullable, text)
import Hasql.Encoders (Params, param)
import Hasql.Encoders qualified as Encoders (nonNullable, text)
import Hasql.Generator.Internal.Database.Sql.Analysis2.Types
  ( ColumnMetadata
      ( ColumnMetadata,
        columnName,
        columnNullConstraint,
        columnType,
        columnUnderlyingType,
        tableName
      ),
    NullabilityConstraint (NotNull, Null),
    PostgresqlType
      ( PgBool,
        PgBytea,
        PgChar,
        PgDate,
        PgFloat4,
        PgFloat8,
        PgInet,
        PgInt2,
        PgInt4,
        PgInt8,
        PgInterval,
        PgJson,
        PgJsonb,
        PgNumeric,
        PgText,
        PgTime,
        PgTimestamp,
        PgTimestamptz,
        PgTimetz,
        PgUnknown,
        PgUuid
      ),
  )
import Hasql.Generator.Internal.Database.Transaction (transaction)
import Hasql.Transaction (Transaction)

-- | Retrieves the 'ColumnMetadata' for each of the columns in the supplied
--   table.
getColumnMetadata ::
  Text ->
  Transaction [ColumnMetadata]
getColumnMetadata tableName = do
  results <- transaction tableAndColumnsSql tableName encoder decoder
  pure $ Vector.toList results
  where
    -- Adapted from https://dba.stackexchange.com/a/75124
    tableAndColumnsSql :: ByteString
    tableAndColumnsSql =
      "select "
        <> "  pga.attname, "
        <> "  format_type(pga.atttypid, pga.atttypmod) as column_type, "
        <> "  coalesce(format_type(pgt.typbasetype, pgt.typtypmod), format_type(pga.atttypid, pga.atttypmod)) as underlying_type,"
        <> "  pga.attnotnull "
        <> "from pg_attribute pga"
        <> "left join pg_catalog.pg_type pgt "
        <> "  on pgt.typname = format_type(pga.atttypid, pga.atttypmod)"
        <> "  and pgt.typtype = 'd' "
        <> "  and pg_type_is_visible(pgt.oid) "
        <> "where pga.attrelid = $1::regclass "
        <> "and pga.attnum > 0 "
        <> "and not pga.attisdropped;"

    encoder :: Params Text
    encoder = param $ Encoders.nonNullable Encoders.text

    decoder :: Result (Vector ColumnMetadata)
    decoder =
      let rows =
            rowVector $
              (,,,)
                <$> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.bool)
       in fmap processRow <$> rows
      where
        processRow :: (Text, Text, Text, Bool) -> ColumnMetadata
        processRow (columnName, rawColumnType, rawUnderlyingType, isNotNull) =
          ColumnMetadata
            { tableName = tableName
            , columnName = columnName
            , columnType = textToPostgresqlType rawColumnType
            , columnUnderlyingType = textToPostgresqlType rawUnderlyingType
            , columnNullConstraint = if isNotNull then NotNull else Null
            }

-- | Converts a text-based representation of a type within Postgres to a
--   'PostgresqlType'.
textToPostgresqlType :: Text -> PostgresqlType
textToPostgresqlType = \case
  --
  "bool" -> PgBool
  "boolean" -> PgBool
  --
  "smallint" -> PgInt2
  "int2" -> PgInt2
  --
  "integer" -> PgInt4
  "int" -> PgInt4
  "int4" -> PgInt4
  --
  "bigint" -> PgInt8
  "int8" -> PgInt8
  --
  "real" -> PgFloat4
  "float4" -> PgFloat4
  --
  "double precision" -> PgFloat8
  "float8" -> PgFloat8
  --
  "numeric" -> PgNumeric
  "decimal" -> PgNumeric
  --
  "character" -> PgChar
  "char" -> PgChar
  --
  "character varying" -> PgText
  "citext" -> PgText
  "varchar" -> PgText
  "text" -> PgText
  --
  "bytea" -> PgBytea
  --
  "date" -> PgDate
  --
  "timestamp" -> PgTimestamp
  --
  "timestamp with time zone" -> PgTimestamptz
  --
  "time" -> PgTime
  --
  "time with time zone" -> PgTimetz
  "timetz" -> PgTimetz
  --
  "interval" -> PgInterval
  --
  "uuid" -> PgUuid
  --
  "inet" -> PgInet
  --
  "json" -> PgJson
  --
  "jsonb" -> PgJsonb
  --
  unknown -> PgUnknown unknown
