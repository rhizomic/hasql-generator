module Hasql.Generator.Internal.Renderer
  ( toHaskell,
  )
where

import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (or)
import Data.Function (flip, ($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (any, concatMap, length, null, sort, zip, (++))
import Data.Map.Strict (Map, lookup)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (Text, append, elem, intercalate, pack, replicate, unwords)
import Data.Tuple (fst, snd)
import GHC.Err (error)
import GHC.Show (show)
import Hasql.Generator.Internal.Database.Sql.Analysis.Types
  ( ColumnMetadata (columnNullConstraint, columnType),
    NullabilityConstraint (NotNull, Null),
    ParameterMetadata (parameterNullConstraint, parameterType),
    ParameterType (ArrayParameter, ScalarParameter),
    PostgresqlParameterAndResultMetadata (numberOfRowsReturned, parameterMetadata, resultMetadata),
    PostgresqlType
      ( PgBool,
        PgBytea,
        PgChar,
        PgDate,
        PgEnum,
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
import Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( NumberOfRowsReturned
      ( AtMostMoreThanOne,
        AtMostOne,
        ExactlyOne,
        None,
        Unknown
      ),
  )
import Hasql.Generator.Types (EnumConfig (haskellType, moduleName))

toHaskell ::
  ByteString ->
  PostgresqlParameterAndResultMetadata ->
  Text ->
  Text ->
  Map Text EnumConfig ->
  Text
toHaskell sql parameterAndResultMetadata moduleName functionName enumConfigs =
  haskellOutput
    parameterAndResultMetadata.parameterMetadata
    (fmap columnMetadataToTuple parameterAndResultMetadata.resultMetadata)
  where
    numberOfRowsReturned :: NumberOfRowsReturned
    numberOfRowsReturned = parameterAndResultMetadata.numberOfRowsReturned

    columnMetadataToTuple ::
      ColumnMetadata ->
      (PostgresqlType, NullabilityConstraint)
    columnMetadataToTuple metadata =
      (metadata.columnType, metadata.columnNullConstraint)

    haskellOutput ::
      [ParameterMetadata] ->
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    haskellOutput parameterMetadata resultTypes =
      "{-# OPTIONS_GHC -Wno-unused-imports #-}\n"
        <> "\n"
        <> (moduleDeclaration <> "\n")
        <> (imports enumConfigs parameterMetadata resultTypes numberOfRowsReturned <> "\n")
        <> "\n"
        <> functionNameAndTypeSignature 2 parameterMetadata resultTypes
        <> "\n"
        <> (functionAndIdentifiers parameterMetadata <> " =\n")
        <> (functionBody <> "\n")
        <> "  where\n"
        <> (sqlClause <> "\n")
        <> "\n"
        <> (paramsClause parameterMetadata <> "\n")
        <> "\n"
        <> (encoderClause parameterMetadata <> "\n")
        <> "\n"
        <> (decoderClause resultTypes <> "\n")

    moduleDeclaration :: Text
    moduleDeclaration =
      ("module " <> moduleName <> "\n")
        <> ("  (" <> functionName <> ")\n")
        <> "where\n"

    functionNameAndTypeSignature ::
      Int ->
      [ParameterMetadata] ->
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    functionNameAndTypeSignature paddingAmount parameterMetadata resultTypes =
      (functionName <> " ::\n")
        <> formattedParameterTypes
        <> formattedResultTypes
      where
        formattedParameterTypes :: Text
        formattedParameterTypes =
          case null parameterMetadata of
            True -> ""
            False ->
              let formatted =
                    intercalate " ->\n" $
                      fmap
                        (append (pad paddingAmount) . parameterMetadataToHaskellType enumConfigs)
                        parameterMetadata
               in formatted <> " ->\n"

        formattedResultTypes :: Text
        formattedResultTypes =
          pad paddingAmount
            <> resultTypeSignature enumConfigs resultTypes numberOfRowsReturned "Transaction"

    identifierNames ::
      [ParameterMetadata] ->
      [Text]
    identifierNames parameterMetadata =
      let upperIndex = length parameterMetadata
       in fmap (\n -> "a" <> pack (show n)) [1 .. upperIndex]

    functionAndIdentifiers ::
      [ParameterMetadata] ->
      Text
    functionAndIdentifiers parameterMetadata =
      case null parameterMetadata of
        True -> functionName
        False -> functionName <> " " <> unwords (identifierNames parameterMetadata)

    functionBody :: Text
    functionBody =
      "  statement\n\
      \   params\n\
      \   (Statement sql encoder decoder True)\n"

    sqlClause :: Text
    sqlClause =
      "    sql :: ByteString\n\
      \    sql = "
        <> pack (show sql)

    paramsClause ::
      [ParameterMetadata] ->
      Text
    paramsClause parameterMetadata =
      let allIdentifierNames = identifierNames parameterMetadata
       in case zip parameterMetadata allIdentifierNames of
            [] ->
              "    params :: ()\n\
              \    params = ()"
            [(metadata, identifierName)] ->
              "    params :: "
                <> parameterMetadataToHaskellType enumConfigs metadata
                <> "\n\
                   \    params = "
                <> identifierName
            _otherwise ->
              "    params :: ("
                <> intercalate ", " (fmap (parameterMetadataToHaskellType enumConfigs) parameterMetadata)
                <> ")\n\
                   \    params = ("
                <> intercalate ", " allIdentifierNames
                <> ")"

    encoderClause ::
      [ParameterMetadata] ->
      Text
    encoderClause parameterMetadata =
      "    encoder :: "
        <> encoderTypeSignature
        <> "\n\
           \    encoder =\n"
        <> encoderBody
      where
        encoderTypeSignature :: Text
        encoderTypeSignature =
          case parameterMetadata of
            [] ->
              "Encoders.Params ()"
            [metadata] ->
              let haskellType = parameterMetadataToHaskellType enumConfigs metadata
               in case ' ' `elem` haskellType of
                    True -> "Encoders.Params (" <> haskellType <> ")"
                    False -> "Encoders.Params " <> haskellType
            _otherwise ->
              "Encoders.Params (" <> intercalate ", " (fmap (parameterMetadataToHaskellType enumConfigs) parameterMetadata) <> ")"

        encoderBody :: Text
        encoderBody =
          case parameterMetadata of
            [] ->
              pad 6 <> "Encoders.noParams"
            [metadata] ->
              toParam 6 False metadata
            _otherwise ->
              let params = intercalate "\n" $ fmap (toParam 8 True) parameterMetadata
               in pad 6
                    <> "contrazip"
                    <> pack (show $ length parameterMetadata)
                    <> "\n"
                    <> params
          where
            toParam ::
              Int ->
              Bool ->
              ParameterMetadata ->
              Text
            toParam paddingAmount withParens metadata =
              let (prefix, suffix) = case withParens of
                    True -> ("(", ")")
                    False -> ("", "")
               in pad paddingAmount
                    <> prefix
                    <> "Encoders.param $ "
                    <> parameterMetadataToEncoder metadata
                    <> suffix

    decoderClause ::
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    decoderClause resultTypes =
      "    decoder :: "
        <> decoderTypeSignature
        <> "\n\
           \    decoder =\n"
        <> decoderBody
      where
        decoderTypeSignature :: Text
        decoderTypeSignature = resultTypeSignature enumConfigs resultTypes numberOfRowsReturned "Decoders.Result"

        decoderBody :: Text
        decoderBody =
          case resultTypes of
            [] ->
              pad 6
                <> resultFunction
            [pgTypeAndConstraint] ->
              pad 6
                <> resultFunction
                <> " $\n"
                <> pad 8
                <> toResult pgTypeAndConstraint
            (firstTypeAndConstraint : remainingTypesAndConstraints) ->
              pad 6
                <> resultFunction
                <> " $\n"
                <> pad 8
                <> "("
                <> replicate (length remainingTypesAndConstraints) ","
                <> ")"
                <> "\n"
                <> pad 10
                <> "<$> "
                <> toResult firstTypeAndConstraint
                <> "\n"
                <> intercalate "\n" (fmap (append (pad 10 <> "<*> ") . toResult) remainingTypesAndConstraints)
          where
            resultFunction :: Text
            resultFunction =
              case numberOfRowsReturned of
                ExactlyOne -> "Decoders.singleRow"
                AtMostOne -> "Decoders.rowMaybe"
                None -> "Decoders.noResult"
                AtMostMoreThanOne -> "Decoders.rowVector"
                Unknown -> "Decoders.rowVector"

            toResult ::
              (PostgresqlType, NullabilityConstraint) ->
              Text
            toResult pgTypeAndConstraint =
              "Decoders.column ("
                <> nullabilityConstraintToNullableDecoder (snd pgTypeAndConstraint)
                <> " "
                <> postgresqlTypeToDecoder (fst pgTypeAndConstraint)
                <> ")"

            nullabilityConstraintToNullableDecoder ::
              NullabilityConstraint ->
              Text
            nullabilityConstraintToNullableDecoder = \case
              NotNull -> "Decoders.nonNullable"
              Null -> "Decoders.nullable"

pad :: Int -> Text
pad amount = replicate amount " "

imports ::
  Map Text EnumConfig ->
  [ParameterMetadata] ->
  [(PostgresqlType, NullabilityConstraint)] ->
  NumberOfRowsReturned ->
  Text
imports enumConfigs parameterMetadata resultTypesAndConstraints numberOfRowsReturned =
  intercalate "\n"
    . nubOrd
    . sort
    $ baseImports <> libImports <> typeImports <> contrazipImports
  where
    baseImports :: [Text]
    baseImports =
      [ "import Control.Applicative ((<*>))"
      , "import Data.Bool (Bool (True))"
      , "import Data.ByteString (ByteString)"
      , "import Data.Either (Either (Right))" -- for handling Decoders.custom
      , "import Data.Function (($))"
      , "import Data.Functor ((<$>))"
      , "import Data.Maybe (Maybe)"
      ]

    libImports :: [Text]
    libImports =
      let vectorImport :: [Text] = case includeVector of
            True -> ["import Data.Vector (Vector)"]
            False -> []
       in vectorImport
            ++ [ "import Hasql.Decoders qualified as Decoders"
               , "import Hasql.Encoders qualified as Encoders"
               , "import Hasql.Statement (Statement (Statement))"
               , "import Hasql.Transaction (Transaction, statement)"
               ]
      where
        includeVector :: Bool
        includeVector =
          or
            [ rowsReturnedRequireVector
            , parametersIncludeArray
            ]
          where
            rowsReturnedRequireVector :: Bool
            rowsReturnedRequireVector = case numberOfRowsReturned of
              ExactlyOne -> False
              AtMostOne -> False
              None -> False
              AtMostMoreThanOne -> True
              Unknown -> True

            parametersIncludeArray :: Bool
            parametersIncludeArray =
              flip any parameterMetadata $ \metadata ->
                case metadata.parameterType of
                  ScalarParameter _type -> False
                  ArrayParameter _type -> True

    typeImports :: [Text]
    typeImports =
      concatMap
        (postgresqlTypeToImports enumConfigs)
        (parameterTypes ++ resultTypes)
      where
        parameterTypes :: [PostgresqlType]
        parameterTypes = fmap (underlyingType . (.parameterType)) parameterMetadata
          where
            underlyingType :: ParameterType -> PostgresqlType
            underlyingType = \case
              ScalarParameter parameterType -> parameterType
              ArrayParameter parameterType -> parameterType

        resultTypes :: [PostgresqlType]
        resultTypes = fmap fst resultTypesAndConstraints

    contrazipImports :: [Text]
    contrazipImports =
      case length parameterMetadata of
        0 -> []
        1 -> []
        n -> ["import Contravariant.Extras (contrazip" <> pack (show n) <> ")"]

parameterMetadataToHaskellType ::
  Map Text EnumConfig ->
  ParameterMetadata ->
  Text
parameterMetadataToHaskellType enumConfigs parameterMetadata =
  let (requiresVector, pgType) = case parameterMetadata.parameterType of
        ArrayParameter underlyingType -> (True, underlyingType)
        ScalarParameter underlyingType -> (False, underlyingType)
      innerType = buildInnerType pgType
   in case (requiresVector, ' ' `elem` innerType) of
        (True, True) -> "Vector (" <> innerType <> ")"
        (True, False) -> "Vector " <> innerType
        _ -> innerType
  where
    buildInnerType :: PostgresqlType -> Text
    buildInnerType pgType =
      maybePrefix parameterMetadata.parameterNullConstraint
        <> postgresqlTypeToHaskellType enumConfigs pgType

    maybePrefix :: NullabilityConstraint -> Text
    maybePrefix = \case
      NotNull -> ""
      Null -> "Maybe "

resultTypeSignature ::
  Map Text EnumConfig ->
  [(PostgresqlType, NullabilityConstraint)] ->
  NumberOfRowsReturned ->
  Text ->
  Text
resultTypeSignature enumConfigs resultTypes numberOfRowsReturned outerType =
  case numberOfRowsReturned of
    None -> outerType <> " ()"
    AtMostMoreThanOne -> multipleResultType
    Unknown -> multipleResultType
    ExactlyOne ->
      case ' ' `elem` innerTypes of
        True -> outerType <> " (" <> innerTypes <> ")"
        False -> outerType <> " " <> innerTypes
    AtMostOne ->
      case ' ' `elem` innerTypes of
        True -> outerType <> " (Maybe (" <> innerTypes <> "))"
        False -> outerType <> " (Maybe " <> innerTypes <> ")"
  where
    innerTypes :: Text
    innerTypes =
      intercalate
        ", "
        (fmap postgresqlTypeAndNullabilityConstraintToHaskellType resultTypes)

    postgresqlTypeAndNullabilityConstraintToHaskellType ::
      (PostgresqlType, NullabilityConstraint) ->
      Text
    postgresqlTypeAndNullabilityConstraintToHaskellType (pgType, constraint) =
      maybePrefix constraint <> postgresqlTypeToHaskellType enumConfigs pgType
      where
        maybePrefix :: NullabilityConstraint -> Text
        maybePrefix = \case
          NotNull -> ""
          Null -> "Maybe "

    multipleResultType :: Text
    multipleResultType =
      case ' ' `elem` innerTypes of
        True -> outerType <> " (Vector (" <> innerTypes <> "))"
        False -> outerType <> " (Vector " <> innerTypes <> ")"

-- https://hackage.haskell.org/package/hasql-1.8.0.1/docs/Hasql-Decoders.html
postgresqlTypeToHaskellType ::
  Map Text EnumConfig ->
  PostgresqlType ->
  Text
postgresqlTypeToHaskellType enumConfigs = \case
  PgBool -> "Bool"
  PgInt2 -> "Int16"
  PgInt4 -> "Int32"
  PgInt8 -> "Int64"
  PgFloat4 -> "Float"
  PgFloat8 -> "Double"
  PgNumeric -> "Scientific"
  PgChar -> "Char"
  PgText -> "Text"
  PgBytea -> "ByteString" -- strict
  PgDate -> "Day"
  PgTimestamp -> "LocalTime"
  PgTimestamptz -> "UTCTime"
  PgTime -> "TimeOfDay"
  PgTimetz -> "(TimeOfDay, TimeZone)"
  PgInterval -> "DiffTime"
  PgUuid -> "UUID"
  PgInet -> "IPRange"
  PgJson -> "Value"
  PgJsonb -> "Value"
  PgEnum enum ->
    case lookup enum enumConfigs of
      Nothing -> error $ "No EnumConfig found for `" <> show enum <> "`."
      Just config -> config.haskellType
  PgUnknown unknown -> unknown

-- https://hackage.haskell.org/package/hasql-1.8.0.1/docs/Hasql-Decoders.html
postgresqlTypeToImports ::
  Map Text EnumConfig ->
  PostgresqlType ->
  [Text]
postgresqlTypeToImports enumConfigs = \case
  PgBool -> ["import Data.Bool (Bool)"]
  PgInt2 -> ["import Data.Int (Int16)"]
  PgInt4 -> ["import Data.Int (Int32)"]
  PgInt8 -> ["import Data.Int (Int64)"]
  PgFloat4 -> ["import GHC.Float (Float)"]
  PgFloat8 -> ["import GHC.Float (Double)"]
  PgNumeric -> ["import Data.Scientific (Scientific)"]
  PgChar -> ["import Data.Char (Char)"]
  PgText -> ["import Data.Text (Text)"]
  PgBytea -> ["import Data.ByteString (ByteString)"] -- strict
  PgDate -> ["import Data.Time.Calendar.OrdinalDate (Day)"]
  PgTimestamp -> ["import Data.Time.LocalTime (LocalTime)"]
  PgTimestamptz -> ["import Data.Time.Clock (UTCTime)"]
  PgTime -> ["import Data.Time.LocalTime (TimeOfDay)"]
  PgTimetz -> ["import Data.Time.LocalTime (TimeOfDay, TimeZone)"]
  PgInterval -> ["import Data.Time.Clock (DiffTime)"]
  PgUuid -> ["import Data.UUID (UUID)"]
  PgInet -> ["import Data.IP (IPRange)"]
  PgJson -> ["import Data.Aeson.Types (Value)"]
  PgJsonb -> ["import Data.Aeson.Types (Value)"]
  PgEnum enum -> maybe [] enumConfigToImport (lookup enum enumConfigs)
  PgUnknown _unknown -> []
  where
    enumConfigToImport :: EnumConfig -> [Text]
    enumConfigToImport config =
      [ "import " <> config.moduleName <> " (" <> config.haskellType <> ")"
      , "import Hasql.Generator.Types (HasqlEnum (hsToPg, pgToHs))"
      ]

-- https://hackage.haskell.org/package/hasql-1.8.0.1/docs/Hasql-Encoders.html
parameterMetadataToEncoder ::
  ParameterMetadata ->
  Text
parameterMetadataToEncoder metadata = case metadata.parameterType of
  ScalarParameter pgType ->
    nullabilityConstraintToNullableEncoder metadata.parameterNullConstraint
      <> " "
      <> pgTypeToEncoder pgType
  ArrayParameter pgType ->
    "Encoders.nonNullable $ Encoders.foldableArray $ "
      <> nullabilityConstraintToNullableEncoder metadata.parameterNullConstraint
      <> " "
      <> pgTypeToEncoder pgType
  where
    pgTypeToEncoder :: PostgresqlType -> Text
    pgTypeToEncoder = \case
      PgBool -> "Encoders.bool"
      PgInt2 -> "Encoders.int2"
      PgInt4 -> "Encoders.int4"
      PgInt8 -> "Encoders.int8"
      PgFloat4 -> "Encoders.float4"
      PgFloat8 -> "Encoders.float8"
      PgNumeric -> "Encoders.numeric"
      PgChar -> "Encoders.char"
      PgText -> "Encoders.text"
      PgBytea -> "Encoders.bytea" -- strict
      PgDate -> "Encoders.date"
      PgTimestamp -> "Encoders.timestamp"
      PgTimestamptz -> "Encoders.timestamptz"
      PgTime -> "Encoders.time"
      PgTimetz -> "Encoders.timetz"
      PgInterval -> "Encoders.interval"
      PgUuid -> "Encoders.uuid"
      PgInet -> "Encoders.inet"
      PgJson -> "Encoders.json"
      PgJsonb -> "Encoders.jsonb"
      PgEnum _enum -> "(Encoders.enum hsToPg)"
      PgUnknown _unknown -> "Encoders.unknown"

    nullabilityConstraintToNullableEncoder ::
      NullabilityConstraint ->
      Text
    nullabilityConstraintToNullableEncoder = \case
      NotNull -> "Encoders.nonNullable"
      Null -> "Encoders.nullable"

-- https://hackage.haskell.org/package/hasql-1.8.0.1/docs/Hasql-Decoders.html
postgresqlTypeToDecoder ::
  PostgresqlType ->
  Text
postgresqlTypeToDecoder = \case
  PgBool -> "Decoders.bool"
  PgInt2 -> "Decoders.int2"
  PgInt4 -> "Decoders.int4"
  PgInt8 -> "Decoders.int8"
  PgFloat4 -> "Decoders.float4"
  PgFloat8 -> "Decoders.float8"
  PgNumeric -> "Decoders.numeric"
  PgChar -> "Decoders.char"
  PgText -> "Decoders.text"
  PgBytea -> "Decoders.bytea" -- strict
  PgDate -> "Decoders.date"
  PgTimestamp -> "Decoders.timestamp"
  PgTimestamptz -> "Decoders.timestamptz"
  PgTime -> "Decoders.time"
  PgTimetz -> "Decoders.timetz"
  PgInterval -> "Decoders.interval"
  PgUuid -> "Decoders.uuid"
  PgInet -> "Decoders.inet"
  PgJson -> "Decoders.json"
  PgJsonb -> "Decoders.jsonb"
  PgEnum _enum -> "(Decoders.enum pgToHs)"
  PgUnknown unknown -> "(Decoders.custom $ \\_ _ -> Right " <> pack (show unknown) <> ")"
