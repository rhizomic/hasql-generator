module Hasql.Generator.Internal.Database.Sql.Analysis2
  ( getColumnMetadata,
  )
where

import Control.Applicative ((<*>))
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Function (($))
import Data.Functor (fmap, (<$>))
import Data.Map.Strict (Map, fromListWith)
import Data.Monoid ((<>))
import Data.Text (Text)
import Hasql.Decoders (Result, column, rowList)
import Hasql.Decoders qualified as Decoders (bool, nonNullable, text)
import Hasql.Encoders (Params, param)
import Hasql.Encoders qualified as Encoders (Value, array, dimension, element, nonNullable, text)
import Hasql.Generator.Internal.Database.Sql.Analysis2.Types
  ( ColumnMetadata
      ( ColumnMetadata,
        columnName,
        columnNullConstraint,
        columnType,
        columnUnderlyingType
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
    TableName (TableName),
  )
import Hasql.Generator.Internal.Database.Transaction (transaction)
import Hasql.Transaction (Transaction)

-- | Retrieves the 'ColumnMetadata' for each of the columns in the supplied
--   tables.
getColumnMetadata ::
  [TableName] ->
  Transaction (Map TableName [ColumnMetadata])
getColumnMetadata tableNames = do
  transaction tableAndColumnsSql (fmap coerce tableNames) encoder decoder
  where
    -- Adapted from https://dba.stackexchange.com/a/75124
    tableAndColumnsSql :: ByteString
    tableAndColumnsSql =
      "select "
        <> "  pgc.relname, "
        <> "  pga.attname, "
        <> "  format_type(pga.atttypid, pga.atttypmod) as column_type, "
        <> "  coalesce(format_type(pgt.typbasetype, pgt.typtypmod), format_type(pga.atttypid, pga.atttypmod)) as underlying_type, "
        <> "  pga.attnotnull "
        <> "from pg_attribute pga "
        <> "join pg_class pgc "
        <> "  on pgc.oid = pga.attrelid "
        <> "left join pg_type pgt "
        <> "  on pgt.typname = format_type(pga.atttypid, pga.atttypmod) "
        <> "  and pgt.typtype = 'd' "
        <> "  and pg_type_is_visible(pgt.oid) "
        <> "where pgc.relname = any ($1) "
        <> "and pga.attnum > 0 "
        <> "and not pga.attisdropped;"

    encoder :: Params [Text]
    encoder =
      let textElement = Encoders.element $ Encoders.nonNullable Encoders.text
          oneDimensionTextArray :: Encoders.Value [Text] =
            Encoders.array $ Encoders.dimension foldl' textElement
       in param $ Encoders.nonNullable oneDimensionTextArray

    decoder :: Result (Map TableName [ColumnMetadata])
    decoder =
      let rows =
            rowList $
              (,,,,)
                <$> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.bool)
          processedRows = fmap processRow <$> rows
       in fmap (fromListWith (<>)) processedRows
      where
        processRow ::
          (Text, Text, Text, Text, Bool) ->
          (TableName, [ColumnMetadata])
        processRow (tableName, columnName, rawColumnType, rawUnderlyingType, isNotNull) =
          ( TableName tableName
          ,
            [ ColumnMetadata
                { columnName = columnName
                , columnType = textToPostgresqlType rawColumnType
                , columnUnderlyingType = textToPostgresqlType rawUnderlyingType
                , columnNullConstraint = if isNotNull then NotNull else Null
                }
            ]
          )

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
