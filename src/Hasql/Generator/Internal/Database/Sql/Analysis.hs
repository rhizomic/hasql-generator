module Hasql.Generator.Internal.Database.Sql.Analysis
  ( getParameterAndResultMetadata,
  )
where

import Control.Applicative (pure, (<*>))
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (elem, foldl')
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (concat, filter, unsnoc, (++))
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import Data.Map.Strict (Map, fromListWith, (!))
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import GHC.Base (error)
import Hasql.Decoders (Result, column, rowList)
import Hasql.Decoders qualified as Decoders (bool, nonNullable, text)
import Hasql.Encoders (Params, param)
import Hasql.Encoders qualified as Encoders (Value, array, dimension, element, nonNullable, text)
import Hasql.Generator.Internal.Database.Sql.Analysis.Types
  ( ColumnMetadata (ColumnMetadata, columnNullConstraint, columnType),
    ColumnReferenceMetadata (ColumnReferenceMetadata, columnNullConstraint, columnReferences, columnType),
    ColumnTypeInformation
      ( ColumnTypeInformation,
        columnName,
        columnNullConstraint,
        columnType,
        columnUnderlyingType
      ),
    NullabilityConstraint (NotNull, Null),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        parameterMetadata,
        resultLimit,
        resultMetadata
      ),
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
import Hasql.Generator.Internal.Database.Sql.Analysis.Types qualified as ColumnReferenceMetadata
  ( ColumnReferenceMetadata (columnNullConstraint),
  )
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( JoinInformation (joinType, tableAndAlias),
    PostgresqlJoinType (FullJoin, InnerJoin, LeftJoin, RightJoin),
    QueryParameter (parameterReference),
    QueryResult (QueryResult),
    TableAndAlias (alias, table),
    TableRelation (BaseTable, JoinTable),
  )
import Hasql.Generator.Internal.Database.Transaction (transaction)
import Hasql.Transaction (Transaction)

getParameterAndResultMetadata ::
  Maybe Int ->
  Maybe (NonEmpty TableRelation) ->
  Maybe (NonEmpty QueryParameter) ->
  Maybe (NonEmpty QueryResult) ->
  Transaction PostgresqlParameterAndResultMetadata
getParameterAndResultMetadata mResultLimit mTableRelations mParameters mResults = do
  columnReferenceMetadata <- maybe (pure []) getColumnReferenceMetadata mTableRelations

  let toColumnMetadata = referenceToColumnMetadata columnReferenceMetadata
      parameters = maybe [] (fmap (.parameterReference) . toList) mParameters
      results = maybe [] (fmap coerce . toList) mResults

  pure
    PostgresqlParameterAndResultMetadata
      { parameterMetadata = fmap toColumnMetadata parameters
      , resultMetadata = fmap toColumnMetadata results
      , resultLimit = mResultLimit
      }
  where
    referenceToColumnMetadata ::
      [ColumnReferenceMetadata] ->
      Text ->
      ColumnMetadata
    referenceToColumnMetadata columnReferenceMetadata reference =
      let referenceMetadata =
            case filter (elem reference . (.columnReferences)) columnReferenceMetadata of
              [] -> error $ "Reference `" <> unpack reference <> "` doesn't point to any known column."
              [x] -> x
              (_x : _xs) -> error $ "Reference `" <> unpack reference <> "` could not be resolved to a single column."
       in toColumnMetadata referenceMetadata
      where
        toColumnMetadata ::
          ColumnReferenceMetadata ->
          ColumnMetadata
        toColumnMetadata referenceMetadata =
          ColumnMetadata
            { columnType = referenceMetadata.columnType
            , columnNullConstraint = referenceMetadata.columnNullConstraint
            }

-- | Retrieves all of the 'ColumnReferenceMetadata' for each of the
--   'TableRelation's.
getColumnReferenceMetadata ::
  NonEmpty TableRelation ->
  Transaction [ColumnReferenceMetadata]
getColumnReferenceMetadata tableRelations = do
  let tableNames = toList $ fmap tableRelationToTableName tableRelations
  allTypeInformation <- getColumnTypeInformation tableNames

  let relationsWithMetadata =
        fmap (toTableRelationAndColumnReferenceMetadata allTypeInformation) tableRelations
      allMetadata = foldl' adjustNullability [] relationsWithMetadata
  pure $ concat allMetadata
  where
    tableRelationToTableName :: TableRelation -> TableName
    tableRelationToTableName =
      TableName . \case
        BaseTable tableAndAlias -> tableAndAlias.table
        JoinTable joinInformation -> joinInformation.tableAndAlias.table

    toTableRelationAndColumnReferenceMetadata ::
      Map TableName [ColumnTypeInformation] ->
      TableRelation ->
      (TableRelation, [ColumnReferenceMetadata])
    toTableRelationAndColumnReferenceMetadata allTypeInformation tableRelation =
      let tableName = tableRelationToTableName tableRelation
          -- TODO: Defend the use of `!` or use something like 'lookup' instead
          tableTypeInformation = allTypeInformation ! tableName
          columnReferenceMetadata = fmap toColumnReferenceMetadata tableTypeInformation
       in (tableRelation, columnReferenceMetadata)
      where
        toColumnReferenceMetadata ::
          ColumnTypeInformation ->
          ColumnReferenceMetadata
        toColumnReferenceMetadata typeInfo =
          let tableAndAlias = case tableRelation of
                BaseTable t -> t
                JoinTable joinInformation -> joinInformation.tableAndAlias
              references = case tableAndAlias.alias of
                Nothing ->
                  fromList
                    [ tableAndAlias.table <> "." <> typeInfo.columnName
                    , typeInfo.columnName
                    ]
                Just alias ->
                  fromList
                    [ alias <> "." <> typeInfo.columnName
                    , typeInfo.columnName
                    ]
           in ColumnReferenceMetadata
                { columnReferences = references
                , columnType = typeInfo.columnUnderlyingType
                , columnNullConstraint = typeInfo.columnNullConstraint
                }

    adjustNullability ::
      [[ColumnReferenceMetadata]] ->
      (TableRelation, [ColumnReferenceMetadata]) ->
      [[ColumnReferenceMetadata]]
    adjustNullability acc (relation, columnReferenceMetadata) =
      case tableRelationToJoinType relation of
        Nothing -> acc ++ [columnReferenceMetadata]
        Just InnerJoin -> acc ++ [columnReferenceMetadata]
        Just LeftJoin -> acc ++ [fmap forceNullable columnReferenceMetadata]
        Just RightJoin -> fmap (fmap forceNullable) acc ++ [columnReferenceMetadata]
        Just FullJoin ->
          case unsnoc acc of
            Nothing ->
              acc ++ [fmap forceNullable columnReferenceMetadata]
            Just (initial, priorColumnReferenceMetadata) ->
              initial
                ++ [ fmap forceNullable priorColumnReferenceMetadata
                   , fmap forceNullable columnReferenceMetadata
                   ]
      where
        tableRelationToJoinType ::
          TableRelation ->
          Maybe PostgresqlJoinType
        tableRelationToJoinType = \case
          BaseTable _tableAndAlias -> Nothing
          JoinTable joinInformation -> Just joinInformation.joinType

        forceNullable ::
          ColumnReferenceMetadata ->
          ColumnReferenceMetadata
        forceNullable metadata =
          metadata {ColumnReferenceMetadata.columnNullConstraint = Null}

-- | Retrieves the 'ColumnTypeInformation' for each of the columns in the
--   supplied tables.
getColumnTypeInformation ::
  [TableName] ->
  Transaction (Map TableName [ColumnTypeInformation])
getColumnTypeInformation tableNames = do
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

    decoder :: Result (Map TableName [ColumnTypeInformation])
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
          (TableName, [ColumnTypeInformation])
        processRow (tableName, columnName, rawColumnType, rawUnderlyingType, isNotNull) =
          ( TableName tableName
          ,
            [ ColumnTypeInformation
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
