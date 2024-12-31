module Hasql.Generator.Internal.Database.Sql.Parser.Types
  ( ColumnReference (..),
    PostgresqlExpression (..),
    PostgresqlJoinType (..),
    TableAndAlias (..),
    JoinInformation (..),
    TableRelation (..),
    QueryParameter (..),
    QueryResult (..),
  )
where

import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Ord (Ord (compare), Ordering)
import Data.Text (Text)
import GHC.Show (Show)

data ColumnReference = ColumnReference
  { tableName :: Maybe Text
  , columnName :: Text
  }

data PostgresqlExpression
  = -- | The expression is a constant. All 'Constant's are `not null` by
    --   definition.
    ConstantExpression
  | -- | The expression is referencing a column.
    ColumnExpression ColumnReference

data PostgresqlJoinType
  = FullJoin
  | LeftJoin
  | RightJoin
  | InnerJoin
  deriving stock (Show, Eq)

-- TODO: Use bespoke types for each of table and alias
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

data QueryParameter = QueryParameter
  { parameterNumber :: Int
  , parameterReference :: Text
  }
  deriving stock (Show, Eq)

instance Ord QueryParameter where
  compare :: QueryParameter -> QueryParameter -> Ordering
  compare x y = compare x.parameterNumber y.parameterNumber

newtype QueryResult = QueryResult Text
  deriving newtype (Show, Eq)
