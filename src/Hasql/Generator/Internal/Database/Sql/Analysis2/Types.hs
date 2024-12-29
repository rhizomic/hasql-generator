module Hasql.Generator.Internal.Database.Sql.Analysis2.Types
  ( PostgresqlType (..),
    NullabilityConstraint (..),
    TableName (..),
    ColumnMetadata (..),
  )
where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Text (Text)
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

-- | Denotes the presence of a `not null` constraint.
data NullabilityConstraint
  = -- | A `not null` constraint is present.
    NotNull
  | -- | A `not null` constraint is absent.
    Null
  deriving stock (Show, Eq)

-- | The name of a table.
newtype TableName = TableName Text
  deriving newtype (Show, Eq, Ord)

-- | Metadata about a particular column in a table.
data ColumnMetadata = ColumnMetadata
  { columnName :: Text
  -- ^ The column name.
  , columnType :: PostgresqlType
  -- ^ The column type, as depicted by e.g., `\d TABLE` in `psql`.
  , columnUnderlyingType :: PostgresqlType
  -- ^ The type underlying the column type. This will only be distinct from
  --   'columnType' if 'columnType' is a domain.
  , columnNullConstraint :: NullabilityConstraint
  -- ^ The column nullability constraint.
  }
  deriving stock (Show, Eq)
