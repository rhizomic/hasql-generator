module Hasql.Generator.Internal.DatabaseSpec (spec) where

import Data.ByteString (readFile)
import Data.Either (Either (Right))
import Data.Maybe (Maybe (Nothing))
import Hasql.Generator.Internal.Database.Sql
  ( parameterAndResultMetadata,
  )
import Hasql.Generator.Internal.Database.Sql.Analysis2.Types
  ( ColumnMetadata (ColumnMetadata, columnNullConstraint, columnType),
    NullabilityConstraint (NotNull, Null),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        parameterMetadata,
        resultLimit,
        resultMetadata
      ),
    PostgresqlType (PgText, PgTimestamptz, PgUuid),
  )
import Hasql.Pool (Pool)
import Test.Hspec
  ( Spec,
    describe,
    it,
    xit,
  )
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Pool -> Spec
spec pool =
  describe "parameterAndResultMetadata" do
    it "returns the correct results for a query that fetches a single table's columns" do
      query <- readFile "test/sql/single_table_columns.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgTimestamptz
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgTimestamptz
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    -- TODO: We're skipping this for now, as we don't support constants in results
    xit "returns the correct results for a query that fetches a single table's columns and a constant" do
      query <- readFile "test/sql/single_table_columns_with_constant.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgTimestamptz
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    -- TODO: We're skipping this for now, as we don't support expressions in results
    xit "returns the correct results for a query that fetches a single table's columns and the value of an expression" do
      query <- readFile "test/sql/single_table_columns_with_expression.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgTimestamptz
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables" do
      query <- readFile "test/sql/multiple_table_columns.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    -- TODO: We're skipping this for now, as we don't support expressions in results
    xit "returns the correct results for a query that fetches columns and the value of an expression from multiple tables" do
      query <- readFile "test/sql/multiple_table_columns_with_expression.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a left join" do
      query <- readFile "test/sql/left_join.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a full join" do
      query <- readFile "test/sql/full_join.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = Null
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from a cross join" do
      query <- readFile "test/sql/cross_join.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata = []
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a right join" do
      query <- readFile "test/sql/right_join.sql"
      results <- parameterAndResultMetadata pool query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnMetadata
                      { columnType = PgUuid
                      , columnNullConstraint = NotNull
                      }
                  ]
              , resultMetadata =
                  [ ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = NotNull
                      }
                  , ColumnMetadata
                      { columnType = PgText
                      , columnNullConstraint = Null
                      }
                  ]
              , resultLimit = Nothing
              }

      results `shouldBe` Right expected
