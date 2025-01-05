module Hasql.Generator.Internal.Renderer
  ( toHaskell,
  )
where

import Data.Bool (Bool (False, True))
import Data.ByteString (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (concatMap, length, null, sort, zip, (++))
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
    (fmap columnMetadataToTuple parameterAndResultMetadata.parameterMetadata)
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
      [(PostgresqlType, NullabilityConstraint)] ->
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    haskellOutput parameterTypes resultTypes =
      "{-# OPTIONS_GHC -Wno-unused-imports #-}\n"
        <> "\n"
        <> (moduleDeclaration <> "\n")
        <> (imports enumConfigs parameterTypes resultTypes numberOfRowsReturned <> "\n")
        <> "\n"
        <> functionNameAndTypeSignature 2 parameterTypes resultTypes
        <> "\n"
        <> (functionAndIdentifiers parameterTypes <> " =\n")
        <> (functionBody <> "\n")
        <> "  where\n"
        <> (sqlClause <> "\n")
        <> "\n"
        <> (paramsClause parameterTypes <> "\n")
        <> "\n"
        <> (encoderClause parameterTypes <> "\n")
        <> "\n"
        <> (decoderClause resultTypes <> "\n")

    moduleDeclaration :: Text
    moduleDeclaration =
      ("module " <> moduleName <> "\n")
        <> ("  (" <> functionName <> ")\n")
        <> "where\n"

    functionNameAndTypeSignature ::
      Int ->
      [(PostgresqlType, NullabilityConstraint)] ->
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    functionNameAndTypeSignature paddingAmount parameterTypes resultTypes =
      (functionName <> " ::\n")
        <> formattedParameterTypes
        <> formattedResultTypes
      where
        formattedParameterTypes :: Text
        formattedParameterTypes =
          case null parameterTypes of
            True -> ""
            False ->
              let formatted =
                    intercalate " ->\n" $
                      fmap
                        (append (pad paddingAmount) . postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs)
                        parameterTypes
               in formatted <> " ->\n"

        formattedResultTypes :: Text
        formattedResultTypes =
          pad paddingAmount <> resultTypeSignature enumConfigs resultTypes numberOfRowsReturned "Transaction"

    identifierNames ::
      [(PostgresqlType, NullabilityConstraint)] ->
      [Text]
    identifierNames parameterTypes =
      let upperIndex = length parameterTypes
       in fmap (\n -> "a" <> pack (show n)) [1 .. upperIndex]

    functionAndIdentifiers ::
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    functionAndIdentifiers parameterTypes =
      case null parameterTypes of
        True -> functionName
        False -> functionName <> " " <> unwords (identifierNames parameterTypes)

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
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    paramsClause parameterTypes =
      let allIdentifierNames = identifierNames parameterTypes
       in case zip parameterTypes allIdentifierNames of
            [] ->
              "    params :: ()\n\
              \    params = ()"
            [(parameterType, identifierName)] ->
              "    params :: "
                <> postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs parameterType
                <> "\n\
                   \    params = "
                <> identifierName
            _otherwise ->
              "    params :: ("
                <> intercalate ", " (fmap (postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs) parameterTypes)
                <> ")\n\
                   \    params = ("
                <> intercalate ", " allIdentifierNames
                <> ")"

    encoderClause ::
      [(PostgresqlType, NullabilityConstraint)] ->
      Text
    encoderClause parameterTypes =
      "    encoder :: "
        <> encoderTypeSignature
        <> "\n\
           \    encoder =\n"
        <> encoderBody
      where
        encoderTypeSignature :: Text
        encoderTypeSignature =
          case parameterTypes of
            [] ->
              "Encoders.Params ()"
            [pgTypeAndConstraint] ->
              let haskellType = postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs pgTypeAndConstraint
               in case ' ' `elem` haskellType of
                    True -> "Encoders.Params (" <> haskellType <> ")"
                    False -> "Encoders.Params " <> haskellType
            _otherwise ->
              "Encoders.Params (" <> intercalate ", " (fmap (postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs) parameterTypes) <> ")"

        encoderBody :: Text
        encoderBody =
          case parameterTypes of
            [] ->
              pad 6 <> "Encoders.noParams"
            [pgTypeAndConstraint] ->
              toParam 6 False pgTypeAndConstraint
            _otherwise ->
              let params = intercalate "\n" $ fmap (toParam 8 True) parameterTypes
               in pad 6
                    <> "contrazip"
                    <> pack (show $ length parameterTypes)
                    <> "\n"
                    <> params
          where
            toParam ::
              Int ->
              Bool ->
              (PostgresqlType, NullabilityConstraint) ->
              Text
            toParam paddingAmount withParens pgTypeAndConstraint =
              let (prefix, suffix) = case withParens of
                    True -> ("(", ")")
                    False -> ("", "")
               in pad paddingAmount
                    <> prefix
                    <> "Encoders.param $ "
                    <> nullabilityConstraintToNullableEncoder (snd pgTypeAndConstraint)
                    <> " "
                    <> postgresqlTypeToEncoder (fst pgTypeAndConstraint)
                    <> suffix

            nullabilityConstraintToNullableEncoder ::
              NullabilityConstraint ->
              Text
            nullabilityConstraintToNullableEncoder = \case
              NotNull -> "Encoders.nonNullable"
              Null -> "Encoders.nullable"

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
  [(PostgresqlType, NullabilityConstraint)] ->
  [(PostgresqlType, NullabilityConstraint)] ->
  NumberOfRowsReturned ->
  Text
imports enumConfigs parameterTypesAndConstraints resultTypesAndConstraints numberOfRowsReturned =
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
        includeVector = case numberOfRowsReturned of
          ExactlyOne -> False
          AtMostOne -> False
          None -> False
          AtMostMoreThanOne -> True
          Unknown -> True

    typeImports :: [Text]
    typeImports =
      concatMap
        (postgresqlTypeToImports enumConfigs . fst)
        (parameterTypesAndConstraints ++ resultTypesAndConstraints)

    contrazipImports :: [Text]
    contrazipImports =
      case length parameterTypesAndConstraints of
        0 -> []
        1 -> []
        n -> ["import Contravariant.Extras (contrazip" <> pack (show n) <> ")"]

postgresqlTypeAndNullabilityConstraintToHaskellType ::
  Map Text EnumConfig ->
  (PostgresqlType, NullabilityConstraint) ->
  Text
postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs (pgType, constraint) =
  maybePrefix constraint <> postgresqlTypeToHaskellType enumConfigs pgType
  where
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
    innerTypes = intercalate ", " (fmap (postgresqlTypeAndNullabilityConstraintToHaskellType enumConfigs) resultTypes)

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
postgresqlTypeToEncoder ::
  PostgresqlType ->
  Text
postgresqlTypeToEncoder = \case
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
