module Hasql.Generator.Internal.Database.Sql.Types
  ( PostgresqlType (..),
    PostgresqlParameterAndResultTypeReplacements (..),
    PostgresqlParameterAndResultMetadata (..),
    ExplainOutput (..),
    ExplainQuery (..),
    Plan (..),
    PreparedStatementDetails (..),
    NullabilityConstraint (..),
    ColumnMetadata (..),
    PostgresqlTypeReplacement (..),
    ExpressionMetadata (..),
    FragmentMetadata (..),
    TableAlias (..),
    PlanParameterMetadata (..),
  )
where

import Control.Applicative ((<*>))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, Value)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Show (Show)

-- | Primitive types in Postgresql which also have representation within Hasql.
--
--   https://www.postgresql.org/docs/current/datatype.html
data PostgresqlType
  = -- | bool / boolean
    PgBool
  | -- | smallint / int2
    PgInt2
  | -- | integer / int / int4
    PgInt4
  | -- | bigint / int8
    PgInt8
  | -- | real / float4
    PgFloat4
  | -- | double precision / float8
    PgFloat8
  | -- | numeric / decimal
    PgNumeric
  | -- | character / char
    PgChar
  | -- | character varying / varchar / text / citext
    PgText
  | -- | bytea
    PgBytea
  | -- | date
    PgDate
  | -- | timestamp
    PgTimestamp
  | -- | timestamp with time zone
    PgTimestamptz
  | -- | time
    PgTime
  | -- | time with time zone / timetz
    PgTimetz
  | -- | interval
    PgInterval
  | -- | uuid
    PgUuid
  | -- | inet
    PgInet
  | -- | json
    PgJson
  | -- | jsonb
    PgJsonb
  | -- | Unknown
    PgUnknown Text
  deriving stock (Show, Eq)

-- | A collection of 'PostgresqlType's, along with their replacement. This is
--   used to swap out 'PgUnknown's if a more accurate type can be determined.
data PostgresqlTypeReplacement = PostgresqlTypeReplacement
  { original :: PostgresqlType
  , replacement :: PostgresqlType
  }
  deriving stock (Show, Eq)

data PostgresqlParameterAndResultTypeReplacements = PostgresqlParameterAndResultTypeReplacements
  { parameterTypes :: [PostgresqlTypeReplacement]
  , resultTypes :: [PostgresqlTypeReplacement]
  }

data TableAlias = TableAlias
  { tableName :: Text
  , alias :: Text
  }
  deriving stock (Show, Eq)

data PlanParameterMetadata = PlanParameterMetadata
  { relationName :: Text
  , alias :: Text
  , indexCond :: Maybe Text
  , filter :: Maybe Text
  }

data Plan = Plan
  { relationName :: Maybe Text
  -- ^ "Relation Name"
  , alias :: Maybe Text
  -- ^ "Alias"
  , output :: [Text]
  -- ^ "Output"
  , indexCond :: Maybe Text
  -- ^ "Index Cond"
  , filter :: Maybe Text
  -- ^ "Filter"
  , plans :: Maybe (NonEmpty Plan)
  -- ^ "Plans"
  }
  deriving stock (Show, Eq)

instance FromJSON Plan where
  parseJSON :: Value -> Parser Plan
  parseJSON = withObject "Plan" $ \v ->
    Plan
      <$> v .:? "Relation Name"
      <*> v .:? "Alias"
      <*> v .: "Output"
      <*> v .:? "Index Cond"
      <*> v .:? "Filter"
      <*> v .:? "Plans"

newtype ExplainQuery = ExplainQuery
  { plan :: Plan
  -- ^ "Plan"
  }
  deriving stock (Show, Eq)

instance FromJSON ExplainQuery where
  parseJSON :: Value -> Parser ExplainQuery
  parseJSON = withObject "ExplainQuery" $ \v ->
    ExplainQuery
      <$> v .: "Plan"

-- | Captures the output from running an `explain verbose` statement.
newtype ExplainOutput = ExplainOutput (NonEmpty ExplainQuery)
  deriving stock (Generic, Show, Eq)

instance FromJSON ExplainOutput

-- | TODO: Docs
data PreparedStatementDetails = PreparedStatementDetails
  { tableAliases :: [TableAlias]
  , parameters :: [Text]
  , results :: [Text]
  }
  deriving stock (Show, Eq)

-- | Denotes the presence of a `not null` constraint.
data NullabilityConstraint
  = -- | A `not null` constraint is present.
    NotNull
  | -- | A `not null` constraint is absent.
    Null
  deriving stock (Show, Eq)

-- | Metadata about a particular column in a table.
data ColumnMetadata = ColumnMetadata
  { tableName :: Text
  -- ^ The table name.
  , aliasName :: Text
  -- ^ The table's alias.
  , columnName :: Text
  -- ^ The column name.
  , columnType :: PostgresqlType
  -- ^ The column type.
  , columnNullConstraint :: NullabilityConstraint
  -- ^ The column nullability constraint.
  }
  deriving stock (Show, Eq)

data ExpressionMetadata = ExpressionMetadata
  { expressionType :: PostgresqlType
  , expressionNullConstraint :: NullabilityConstraint
  }
  deriving stock (Show, Eq)

data FragmentMetadata
  = ColumnFragment ColumnMetadata
  | ExpressionFragment ExpressionMetadata
  | UnknownFragment Text
  deriving stock (Show, Eq)

-- | A collection of parameter and result metadata for a given query.
data PostgresqlParameterAndResultMetadata = PostgresqlParameterAndResultMetadata
  { parameterMetadata :: [FragmentMetadata]
  , resultMetadata :: [FragmentMetadata]
  }
  deriving stock (Show, Eq)
