module Hasql.Generator.Internal.Database.Sql.Analysis
  ( getParameterAndResultTypes,
    getPreparedStatementDetails,
    getParameterAndResultMetadata,
  )
where

import Control.Applicative (pure, (<*>))
import Control.Monad (mapM, replicateM_)
import Control.Monad.Extra (concatMapM)
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor, bimap, second)
import Data.Bool (Bool (False, True), (&&))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (empty)
import Data.Coerce (coerce)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (Either (Left, Right))
import Data.Eq ((==))
import Data.Foldable (concatMap, foldl')
import Data.Function (const, ($), (.))
import Data.Functor (fmap, (<$>))
import Data.List (elem, find, unsnoc, zip, (++))
import Data.List qualified as List (filter)
import Data.List.NonEmpty (NonEmpty, head)
import Data.Map.Strict (Map, elems, empty, singleton, union)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, mapMaybe, maybe)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate, replace)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Tuple (fst, snd)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (toList)
import GHC.Base (error)
import Hasql.Decoders (Result, column, listArray, rowMaybe, rowVector, singleRow)
import Hasql.Decoders qualified as Decoders (Value, bool, bytea, nonNullable, text)
import Hasql.Encoders (Params, param)
import Hasql.Encoders qualified as Encoders (nonNullable, text)
import Hasql.Generator.Internal.Database.Sql.Parser (parseAsExpression)
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( ColumnReference
      ( columnName,
        tableName
      ),
    JoinInformation (joinType, tableAndAlias),
    PostgresqlExpression
      ( ColumnExpression,
        ConstantExpression
      ),
    PostgresqlJoinType (CrossJoin, FullJoin, InnerJoin, LeftJoin, RightJoin),
    TableAndAlias (alias),
    TableRelation (BaseTable, JoinTable),
  )
import Hasql.Generator.Internal.Database.Sql.Types
  ( ColumnMetadata
      ( ColumnMetadata,
        aliasName,
        columnName,
        columnNullConstraint,
        columnType,
        tableName
      ),
    ExplainOutput (ExplainOutput),
    ExplainQuery
      ( plan
      ),
    ExpressionMetadata (ExpressionMetadata, expressionNullConstraint, expressionType),
    FragmentMetadata
      ( ColumnFragment,
        ExpressionFragment,
        UnknownFragment
      ),
    NullabilityConstraint (NotNull, Null),
    Plan
      ( alias,
        filter,
        indexCond,
        output,
        plans,
        relationName
      ),
    PlanParameterMetadata
      ( PlanParameterMetadata,
        alias,
        filter,
        indexCond,
        relationName
      ),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        parameterMetadata,
        resultMetadata
      ),
    PostgresqlParameterAndResultTypeReplacements
      ( PostgresqlParameterAndResultTypeReplacements,
        parameterTypes,
        resultTypes
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
    PostgresqlTypeReplacement
      ( PostgresqlTypeReplacement,
        original,
        replacement
      ),
    PreparedStatementDetails
      ( PreparedStatementDetails,
        parameters,
        results,
        tableAliases
      ),
    TableAlias
      ( TableAlias,
        alias,
        tableName
      ),
  )
import Hasql.Generator.Internal.Database.Transaction (paramlessTransaction, transaction)
import Hasql.Transaction (Transaction)
import Text.Regex.PCRE.Heavy (Regex, compileM, scan)

-- | Retrieves the types corresponding to the parameters (e.g., `$1`, `$2`)
--   involved in the supplied prepared statement.
getParameterAndResultTypes ::
  ByteString ->
  Transaction PostgresqlParameterAndResultTypeReplacements
getParameterAndResultTypes preparedStatementName = do
  (params, results) <-
    transaction
      sql
      (decodeUtf8 preparedStatementName)
      encoder
      decoder

  parameterTypes <- getTypeReplacements params
  resultTypes <- getTypeReplacements results

  pure $
    PostgresqlParameterAndResultTypeReplacements
      { parameterTypes
      , resultTypes
      }
  where
    sql :: ByteString
    sql =
      "select "
        <> "parameter_types::text[], result_types::text[] "
        <> "from pg_prepared_statements "
        <> "where name = $1"

    encoder :: Params Text
    encoder = param $ Encoders.nonNullable Encoders.text

    decoder :: Result ([PostgresqlType], [PostgresqlType])
    decoder =
      let row =
            singleRow $
              (,)
                <$> column (Decoders.nonNullable nonNullTextArray)
                <*> column (Decoders.nonNullable nonNullTextArray)
       in mapBoth (fmap textToPostgresqlType) <$> row
      where
        nonNullTextArray :: Decoders.Value [Text]
        nonNullTextArray = listArray $ Decoders.nonNullable Decoders.text

        mapBoth :: (Bifunctor p) => (a -> b) -> p a a -> p b b
        mapBoth f = bimap f f

-- | Retrieves the output returned from running an `explain verbose` statement
--   against the supplied prepared statement. Note that `PostgresqlType`s are
--   necessary for the statement to be executed properly.
getExplainOutput ::
  ByteString ->
  [PostgresqlType] ->
  Transaction ByteString
getExplainOutput preparedStatementName parameterTypes =
  paramlessTransaction sql decoder
  where
    sql :: ByteString
    sql =
      "explain (format json, verbose) "
        <> "execute "
        <> preparedStatementName
        <> statementArgs
      where
        statementArgs :: ByteString
        statementArgs =
          let args =
                encodeUtf8 $
                  intercalate ", " $
                    fmap postgresqlTypeToPlaceholderValue parameterTypes
           in case args == BS.empty of
                True -> args
                False -> " (" <> args <> ")"

    decoder :: Result ByteString
    decoder = singleRow $ column (Decoders.nonNullable Decoders.bytea)

parseAsExplainOutput :: ByteString -> ExplainOutput
parseAsExplainOutput queryResult =
  case eitherDecodeStrict queryResult of
    Left err ->
      -- TODO: This should be removed before releasing this library
      error ("Parsing result from EXPLAIN failed: " <> err)
    Right res -> res

-- | Retrieves the 'ColumnMetadata' for each of the columns in the supplied
--   table.
getColumnMetadata ::
  TableAlias ->
  Transaction [ColumnMetadata]
getColumnMetadata TableAlias {tableName, alias} = do
  results <- transaction tableAndColumnsSql tableName encoder decoder
  pure $ Vector.toList results
  where
    -- Adapted from https://dba.stackexchange.com/a/75124
    tableAndColumnsSql :: ByteString
    tableAndColumnsSql =
      "select "
        <> "  attname, "
        <> "  format_type(atttypid, atttypmod), "
        <> "  attnotnull "
        <> "from pg_attribute "
        <> "where attrelid = $1::regclass "
        <> "and attnum > 0 "
        <> "and not attisdropped"

    encoder :: Params Text
    encoder = param $ Encoders.nonNullable Encoders.text

    decoder :: Result (Vector ColumnMetadata)
    decoder =
      let rows =
            rowVector $
              (,,)
                <$> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.text)
                <*> column (Decoders.nonNullable Decoders.bool)
       in fmap processRow <$> rows
      where
        processRow :: (Text, Text, Bool) -> ColumnMetadata
        processRow (columnName, rawType, isNotNull) =
          ColumnMetadata
            { tableName = tableName
            , aliasName = alias
            , columnName = columnName
            , columnType = textToPostgresqlType rawType
            , columnNullConstraint = if isNotNull then NotNull else Null
            }

getPreparedStatementDetails ::
  ByteString ->
  [PostgresqlType] ->
  Transaction PreparedStatementDetails
getPreparedStatementDetails preparedStatementName parameterTypes = do
  -- Note that we need to run 'getExplainOutput' a total of six times so that
  -- the output contains the positional parameters (e.g., `$1`, `$2`) instead
  -- of the placeholder values that are passed in. This is necessary for later
  -- parsing to be accurate.
  replicateM_ 5 (getExplainOutput preparedStatementName parameterTypes)

  result <- getExplainOutput preparedStatementName parameterTypes
  let explainOutput = parseAsExplainOutput result

  pure $ explainOutputToPreparedStatementDetails explainOutput
  where
    explainOutputToPreparedStatementDetails ::
      ExplainOutput ->
      PreparedStatementDetails
    explainOutputToPreparedStatementDetails explainOutput =
      let queries :: NonEmpty ExplainQuery = coerce explainOutput
          -- Postgresql returns a list of objects. In practice, it's always a
          -- singleton, so we can just use 'head' and ignore the rest.
          query = head queries
          aliasesAndMetadatas = planToTableAliasesAndPlanParameterMetadatas query.plan

          tableAliases = fmap fst aliasesAndMetadatas
          parametersWithAliases = elems $ foldl' union empty (fmap snd aliasesAndMetadatas)

          replaceAliases = fmap (replaceAliasesWithTableNames tableAliases)

          parameters = nubOrd $ replaceAliases parametersWithAliases
          results = replaceAliases query.plan.output
       in PreparedStatementDetails
            { tableAliases
            , parameters
            , results
            }
      where
        planToTableAliasesAndPlanParameterMetadatas ::
          Plan ->
          [(TableAlias, Map Text Text)]
        planToTableAliasesAndPlanParameterMetadatas plan =
          case (plan.relationName, plan.alias, plan.plans) of
            (Just relationName, Just alias, Nothing) ->
              [ toTableAliasAndPlanParameterMetadata
                  relationName
                  alias
                  plan.indexCond
                  plan.filter
              ]
            (Nothing, Nothing, Just plans) ->
              concatMap planToTableAliasesAndPlanParameterMetadatas plans
            _ -> []

        toTableAliasAndPlanParameterMetadata ::
          Text ->
          Text ->
          Maybe Text ->
          Maybe Text ->
          (TableAlias, Map Text Text)
        toTableAliasAndPlanParameterMetadata relationName alias indexCond filter =
          let tableAlias =
                TableAlias
                  { tableName = relationName
                  , alias = alias
                  }

              planParameterMetadata =
                PlanParameterMetadata
                  { relationName = relationName
                  , alias = alias
                  , indexCond = indexCond
                  , filter = filter
                  }

              parametersWithAliases = planParameterMetadataToParameters planParameterMetadata
           in (tableAlias, parametersWithAliases)

        planParameterMetadataToParameters :: PlanParameterMetadata -> Map Text Text
        planParameterMetadataToParameters metadata =
          let parameterFragments =
                fromMaybe "" metadata.indexCond
                  <> fromMaybe "" metadata.filter
           in columnNames metadata.relationName metadata.alias parameterFragments

        columnNames :: Text -> Text -> Text -> Map Text Text
        columnNames tableName alias parameters =
          let maps = mapMaybe (toMap . snd) $ scan regex parameters
           in foldl' union empty maps
          where
            -- Assembles a regular expression that, when given something like
            -- `(a.user_id = $1)`, will create two capturing groups:
            --
            -- Group 1: table.column (in this case, `a.user_id`)
            -- Group 2: $ number (in this case, `1`)
            regex :: Regex
            regex =
              let expression = "\\(((?:" <> encodeUtf8 tableName <> "|" <> encodeUtf8 alias <> ")\\.\\w+).*?\\$(\\d)+\\)"
               in case compileM expression [] of
                    Left err -> error ("Couldn't compile regex: " <> err)
                    Right r -> r

            toMap :: [a] -> Maybe (Map a a)
            toMap list =
              case list of
                [x, y] -> Just $ singleton y x
                _ -> Nothing

        replaceAliasesWithTableNames :: [TableAlias] -> Text -> Text
        replaceAliasesWithTableNames tableAliases text =
          foldl' replaceAliasWithTableName text tableAliases

        replaceAliasWithTableName :: Text -> TableAlias -> Text
        replaceAliasWithTableName text tableAlias =
          replace (tableAlias.alias <> ".") (tableAlias.tableName <> ".") text

getParameterAndResultMetadata ::
  PostgresqlParameterAndResultTypeReplacements ->
  Maybe (NonEmpty TableRelation) ->
  PreparedStatementDetails ->
  Transaction PostgresqlParameterAndResultMetadata
getParameterAndResultMetadata parameterAndResultTypes mJoins preparedStatementDetails = do
  allColumnMetadata <- concatMapM getColumnMetadata preparedStatementDetails.tableAliases

  let parameterColumnsWithTypes =
        zip
          preparedStatementDetails.parameters
          parameterAndResultTypes.parameterTypes

      resultColumnsWithTypes =
        zip
          preparedStatementDetails.results
          parameterAndResultTypes.resultTypes

  let parameterMetadata =
        fmap (toFragmentMetadata allColumnMetadata) parameterColumnsWithTypes
      resultMetadata =
        fmap (toFragmentMetadata allColumnMetadata) resultColumnsWithTypes

      parameterAndResultMetadata =
        PostgresqlParameterAndResultMetadata
          { parameterMetadata
          , resultMetadata
          }

  replacedMetadata <- replaceUnknownTypesInMetadata parameterAndResultMetadata

  case mJoins of
    Nothing -> pure replacedMetadata
    Just joins -> pure $ updateNullabilityBasedOnJoins replacedMetadata joins
  where
    toFragmentMetadata ::
      [ColumnMetadata] ->
      (Text, PostgresqlTypeReplacement) ->
      FragmentMetadata
    toFragmentMetadata allMetadata (fragment, typeReplacement) =
      case parseAsExpression fragment of
        Just ConstantExpression ->
          ExpressionFragment $
            ExpressionMetadata
              { expressionType = typeReplacement.replacement
              , expressionNullConstraint = NotNull
              }
        Just (ColumnExpression columnReference) ->
          case columnToMetadata allMetadata columnReference of
            Just columnMetadata -> ColumnFragment columnMetadata
            Nothing -> UnknownFragment fragment
        Nothing ->
          UnknownFragment fragment

    columnToMetadata ::
      [ColumnMetadata] ->
      ColumnReference ->
      Maybe ColumnMetadata
    columnToMetadata allMetadata columnReference =
      find metadataMatches allMetadata
      where
        metadataMatches :: ColumnMetadata -> Bool
        metadataMatches metadata =
          case columnReference.tableName of
            Nothing ->
              -- If the query involves no joins, then the 'ColumnReference'
              -- will not have a 'tableName'. Matching on 'columnName' alone is
              -- sufficient.
              metadata.columnName == columnReference.columnName
            Just tableName ->
              -- If the query involves joins, then the 'ColumnReference' will
              -- have a 'tableName'. In order to disambiguate between columns
              -- with the same name, we'll need to also match on 'tableName'.
              metadata.columnName == columnReference.columnName
                && metadata.tableName == tableName

updateNullabilityBasedOnJoins ::
  PostgresqlParameterAndResultMetadata ->
  NonEmpty TableRelation ->
  PostgresqlParameterAndResultMetadata
updateNullabilityBasedOnJoins parameterAndResultMetadata tableRelations =
  PostgresqlParameterAndResultMetadata
    { parameterMetadata = fmap updateNullabilityOfFragmentMetadata parameterAndResultMetadata.parameterMetadata
    , resultMetadata = fmap updateNullabilityOfFragmentMetadata parameterAndResultMetadata.resultMetadata
    }
  where
    tableAliasesForcingNullability :: [Text]
    tableAliasesForcingNullability =
      let allTableAliases = foldl' toTableAliasAndForcedNullability [] tableRelations
       in fmap fst $ List.filter snd allTableAliases
      where
        -- Note that we use the table alias (and not the name) because the same
        -- table can be joined multiple times in one query, and the only way
        -- to differentiate them is with an alias.
        toTableAliasAndForcedNullability ::
          [(Text, Bool)] ->
          TableRelation ->
          [(Text, Bool)]
        toTableAliasAndForcedNullability acc tableRelation =
          case tableRelationToJoinType tableRelation of
            Nothing -> acc ++ [(tableRelationToTableAlias tableRelation, False)]
            Just joinType -> case joinType of
              InnerJoin ->
                acc
                  ++ [(tableRelationToTableAlias tableRelation, False)]
              CrossJoin ->
                acc
                  ++ [(tableRelationToTableAlias tableRelation, False)]
              LeftJoin ->
                acc
                  ++ [(tableRelationToTableAlias tableRelation, True)]
              FullJoin ->
                case unsnoc acc of
                  Nothing ->
                    acc
                      ++ [(tableRelationToTableAlias tableRelation, True)]
                  Just (initial, lastRelation) ->
                    initial
                      ++ [ (fst lastRelation, True)
                         , (tableRelationToTableAlias tableRelation, True)
                         ]
              RightJoin ->
                fmap (second (const True)) acc
                  ++ [(tableRelationToTableAlias tableRelation, False)]

        tableRelationToJoinType ::
          TableRelation ->
          Maybe PostgresqlJoinType
        tableRelationToJoinType = \case
          BaseTable _tableAndAlias -> Nothing
          JoinTable joinInformation -> Just joinInformation.joinType

        tableRelationToTableAlias ::
          TableRelation ->
          Text
        tableRelationToTableAlias = \case
          BaseTable tableAndAlias -> tableAndAlias.alias
          JoinTable joinInformation -> joinInformation.tableAndAlias.alias

    updateNullabilityOfFragmentMetadata ::
      FragmentMetadata ->
      FragmentMetadata
    updateNullabilityOfFragmentMetadata fragmentMetadata = case fragmentMetadata of
      ExpressionFragment _expressionMetadata -> fragmentMetadata
      UnknownFragment _fragment -> fragmentMetadata
      ColumnFragment columnMetadata ->
        ColumnFragment $ updateNullabilityOfColumnMetadata columnMetadata
      where
        updateNullabilityOfColumnMetadata ::
          ColumnMetadata ->
          ColumnMetadata
        updateNullabilityOfColumnMetadata columnMetadata =
          case columnMetadata.aliasName `elem` tableAliasesForcingNullability of
            True -> columnMetadata {columnNullConstraint = Null}
            False -> columnMetadata

replaceUnknownTypesInMetadata ::
  PostgresqlParameterAndResultMetadata ->
  Transaction PostgresqlParameterAndResultMetadata
replaceUnknownTypesInMetadata parameterAndResultMetadata = do
  let allColumnTypes =
        fmap
          fragmentMetadataPgType
          (parameterAndResultMetadata.parameterMetadata ++ parameterAndResultMetadata.resultMetadata)

  replacementTypes <- getTypeReplacements allColumnTypes

  let withReplacedTypes = fmap (replaceUnknownTypeInFragmentMetadata replacementTypes)
      parameterMetadata = withReplacedTypes parameterAndResultMetadata.parameterMetadata
      resultMetadata = withReplacedTypes parameterAndResultMetadata.resultMetadata

  pure
    PostgresqlParameterAndResultMetadata
      { parameterMetadata
      , resultMetadata
      }
  where
    fragmentMetadataPgType ::
      FragmentMetadata ->
      PostgresqlType
    fragmentMetadataPgType = \case
      ColumnFragment columnMetadata -> columnMetadata.columnType
      ExpressionFragment expressionMetadata -> expressionMetadata.expressionType
      UnknownFragment fragment -> PgUnknown fragment

    replaceUnknownTypeInFragmentMetadata ::
      [PostgresqlTypeReplacement] ->
      FragmentMetadata ->
      FragmentMetadata
    replaceUnknownTypeInFragmentMetadata replacementTypes fragmentMetadata =
      case fragmentMetadata of
        ColumnFragment columnMetadata ->
          ColumnFragment $ replaceUnknownTypeInColumnMetadata replacementTypes columnMetadata
        ExpressionFragment expressionMetadata ->
          ExpressionFragment $ replaceUnknownTypeInExpressionMetadata replacementTypes expressionMetadata
        UnknownFragment fragment ->
          UnknownFragment fragment

    replaceUnknownTypeInColumnMetadata ::
      [PostgresqlTypeReplacement] ->
      ColumnMetadata ->
      ColumnMetadata
    replaceUnknownTypeInColumnMetadata replacementTypes columnMetadata =
      columnMetadata
        { columnType = replaceType replacementTypes columnMetadata.columnType
        }

    replaceUnknownTypeInExpressionMetadata ::
      [PostgresqlTypeReplacement] ->
      ExpressionMetadata ->
      ExpressionMetadata
    replaceUnknownTypeInExpressionMetadata replacementTypes expressionMetadata =
      expressionMetadata
        { expressionType = replaceType replacementTypes expressionMetadata.expressionType
        }

    replaceType ::
      [PostgresqlTypeReplacement] ->
      PostgresqlType ->
      PostgresqlType
    replaceType replacementTypes pgType =
      let mReplacement = find ((== pgType) . (.original)) replacementTypes
       in maybe pgType (.replacement) mReplacement

getTypeReplacements ::
  [PostgresqlType] ->
  Transaction [PostgresqlTypeReplacement]
getTypeReplacements types = do
  let unknownTypes = nubOrd $ mapMaybe unwrapUnknown types
  domainReplacements <- mapM underlyingTypeOfDomain unknownTypes
  let unknownReplacements = zip unknownTypes domainReplacements

  pure $ fmap (replaceUnknownWith unknownReplacements) types
  where
    unwrapUnknown :: PostgresqlType -> Maybe Text
    unwrapUnknown (PgUnknown unknown) = Just unknown
    unwrapUnknown _ = Nothing

    replaceUnknownWith ::
      [(Text, PostgresqlType)] ->
      PostgresqlType ->
      PostgresqlTypeReplacement
    replaceUnknownWith replacements pgType = case pgType of
      PgUnknown unknown ->
        let mMatch = find ((== unknown) . fst) replacements
         in case mMatch of
              Nothing -> noReplacement
              Just match ->
                PostgresqlTypeReplacement
                  { original = pgType
                  , replacement = snd match
                  }
      _ -> noReplacement
      where
        noReplacement =
          PostgresqlTypeReplacement
            { original = pgType
            , replacement = pgType
            }

-- | Given a Postgresql domain name, attempts to find its underlying type.
underlyingTypeOfDomain :: Text -> Transaction PostgresqlType
underlyingTypeOfDomain domain = do
  mRow <- transaction sql domain encoder decoder

  case mRow of
    Nothing -> pure $ PgUnknown domain
    Just row -> pure $ textToPostgresqlType row
  where
    sql :: ByteString
    sql =
      "select pg_catalog.format_type(t.typbasetype, t.typtypmod) "
        <> "from pg_catalog.pg_type t "
        <> "inner join pg_catalog.pg_namespace n ON n.oid = t.typnamespace "
        <> "where t.typtype = 'd' "
        <> "and t.typname = $1 "
        <> "and pg_catalog.pg_type_is_visible(t.oid) "
        <> "limit 1"

    encoder :: Params Text
    encoder = param $ Encoders.nonNullable Encoders.text

    decoder :: Result (Maybe Text)
    decoder = rowMaybe $ column (Decoders.nonNullable Decoders.text)

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

-- | Given a 'PostgresqlType', returns a "placeholder" value.
--
--   In order to obtain detailed information about a prepared statement (via
--   `explain verbose`), it's necessary to execute the prepared statement with
--   non-null values. (Note that it doesn't matter what the values are, just
--   that they are non-null.) These placeholders can be used to that end.
postgresqlTypeToPlaceholderValue :: PostgresqlType -> Text
postgresqlTypeToPlaceholderValue = \case
  PgBool -> "true"
  PgInt2 -> "0"
  PgInt4 -> "0"
  PgInt8 -> "0"
  PgFloat4 -> "0"
  PgFloat8 -> "0"
  PgNumeric -> "0"
  PgChar -> "'a'"
  PgText -> "'a'"
  PgBytea -> "'a'"
  PgDate -> "'2000-01-01'"
  PgTimestamp -> "'2000-01-01 00:00:00'"
  PgTimestamptz -> "'2000-01-01 00:00:00+01'"
  PgTime -> "'00:00:00'"
  PgTimetz -> "'00:00:00+01'"
  PgInterval -> "'1 year'"
  PgUuid -> "'00000000-0000-0000-0000-000000000000'"
  PgInet -> "'0.0.0.0'"
  PgJson -> "'{}'"
  PgJsonb -> "'{}'"
  PgUnknown _unknown ->
    -- `explain verbose` will only show us the full details about a query if
    -- we execute a prepared statement with non-null params. In the absence of
    -- anything better, use an empty string.
    "''"
