module Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( PostgresqlJoinType (..),
    TableAndAlias (..),
    JoinInformation (..),
    TableRelation (..),
    QueryParameterAttribute (..),
    QueryParameter (..),
    QueryResult (..),
    NumberOfRowsReturned (..),
  )
where

import Data.Eq (Eq)
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)
import Data.Ord (Ord (compare), Ordering)
import Data.Text (Text)
import GHC.Show (Show)

-- TODO: Docs

data PostgresqlJoinType
  = FullJoin
  | LeftJoin
  | RightJoin
  | InnerJoin
  deriving stock (Show, Eq)

data TableAndAlias = TableAndAlias
  { table :: Text
  , alias :: Maybe Text
  }
  deriving stock (Show, Eq)

data JoinInformation = JoinInformation
  { tableAndAlias :: TableAndAlias
  , joinType :: PostgresqlJoinType
  }
  deriving stock (Show, Eq)

data TableRelation
  = BaseTable TableAndAlias
  | JoinTable JoinInformation
  deriving stock (Show, Eq)

data QueryParameterAttribute
  = -- | The query parameter represents an array of values.
    ParameterIsArray
  deriving stock (Show, Eq)

data QueryParameter = QueryParameter
  { parameterNumber :: Int
  , parameterReference :: Text
  , parameterAttributes :: Maybe (NonEmpty QueryParameterAttribute)
  }
  deriving stock (Show, Eq)

instance Ord QueryParameter where
  compare :: QueryParameter -> QueryParameter -> Ordering
  compare x y = compare x.parameterNumber y.parameterNumber

newtype QueryResult = QueryResult Text
  deriving newtype (Show, Eq)

-- | The number of rows returned by a query.
data NumberOfRowsReturned
  = -- | Exactly one row is returned. This is most common with an INSERT
    --   statement.
    ExactlyOne
  | -- | Either zero or one rows are returned.
    AtMostOne
  | -- | Zero, one, or more rows are returned.
    AtMostMoreThanOne
  | -- | Zero rows are returned.
    None
  | -- | Nothing in the query indicated how many rows would be returned.
    Unknown
  deriving stock (Show, Eq)
