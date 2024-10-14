module Hasql.Generator.Internal.DatabaseSpec (spec) where

import Data.ByteString (readFile)
import Data.Either (Either (Right))
import Data.Function (($), (.))
import Hasql.Generator.Internal.Database.Sql
  ( parameterAndResultMetadata,
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
    ExpressionMetadata
      ( ExpressionMetadata,
        expressionNullConstraint,
        expressionType
      ),
    FragmentMetadata
      ( ColumnFragment,
        ExpressionFragment,
        UnknownFragment
      ),
    NullabilityConstraint (NotNull, Null),
    PostgresqlParameterAndResultMetadata
      ( PostgresqlParameterAndResultMetadata,
        parameterMetadata,
        resultMetadata
      ),
    PostgresqlType
      ( PgText,
        PgTimestamptz,
        PgUuid
      ),
  )
import Hasql.Generator.Internal.Database.Transaction (runTransaction)
import Hasql.Pool (Pool, use)
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Pool -> Spec
spec pool =
  describe "parameterAndResultMetadata" do
    it "returns the correct results for a query that fetches a single table's columns" do
      query <- readFile "test/sql/single_table_columns.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "email"
                        , columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "created_at"
                        , columnType = PgTimestamptz
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "updated_at"
                        , columnType = PgTimestamptz
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches a single table's columns and a constant" do
      query <- readFile "test/sql/single_table_columns_with_constant.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "email"
                        , columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                        , columnNullConstraint = NotNull
                        }
                  , ExpressionFragment
                      ExpressionMetadata
                        { expressionType = PgText
                        , expressionNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "updated_at"
                        , columnType = PgTimestamptz
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches a single table's columns and the value of an expression" do
      query <- readFile "test/sql/single_table_columns_with_expression.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "email"
                        , columnType = PgText -- this is the email, which is actually "citext", which, in turn, is "text"
                        , columnNullConstraint = NotNull
                        }
                  , UnknownFragment "('foo'::text || (name)::text)"
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "users"
                        , columnName = "updated_at"
                        , columnType = PgTimestamptz
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables" do
      query <- readFile "test/sql/multiple_table_columns.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_2"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "country"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns and the value of an expression from multiple tables" do
      query <- readFile "test/sql/multiple_table_columns_with_expression.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "country"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , UnknownFragment "('foo'::text || (users.name)::text)"
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_2"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "country"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a left join" do
      query <- readFile "test/sql/left_join.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "nicknames"
                        , aliasName = "n"
                        , columnName = "nickname"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a full join" do
      query <- readFile "test/sql/full_join.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "id"
                        , columnType = PgUuid
                        , columnNullConstraint = Null
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from a cross join" do
      query <- readFile "test/sql/cross_join.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata = []
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  ]
              }

      results `shouldBe` Right expected

    it "returns the correct results for a query that fetches columns from multiple tables via a right join" do
      query <- readFile "test/sql/right_join.sql"
      results <- use pool . runTransaction $ do
        parameterAndResultMetadata query

      let expected =
            PostgresqlParameterAndResultMetadata
              { parameterMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "user_id"
                        , columnType = PgUuid
                        , columnNullConstraint = NotNull
                        }
                  ]
              , resultMetadata =
                  [ ColumnFragment
                      ColumnMetadata
                        { tableName = "users"
                        , aliasName = "u"
                        , columnName = "name"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "addresses"
                        , aliasName = "a"
                        , columnName = "line_1"
                        , columnType = PgText
                        , columnNullConstraint = NotNull
                        }
                  , ColumnFragment
                      ColumnMetadata
                        { tableName = "nicknames"
                        , aliasName = "n"
                        , columnName = "nickname"
                        , columnType = PgText
                        , columnNullConstraint = Null
                        }
                  ]
              }

      results `shouldBe` Right expected
